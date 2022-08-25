use std::borrow::Cow;
use std::env;
use std::error::Error;
use std::ffi::OsStr;
use std::fmt::Display;
use std::fs::{DirEntry, File as IOFile};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use log::{info, warn};

use crate::misc::{ErrorNodes, try_or_return, error};

pub enum AssociatedIOFile<T> {
    ErrorReading {
        associated_name: &'static str,
        message: String
    },
    CheckExpected {
        associated_name: &'static str,
        expected: T
    },
    WriteExpected {
        associated_name: &'static str,
        base_name: String,
        expected_file: IOFile
    }
}

impl<T> AssociatedIOFile<T> {
    fn write_expected(associated_name: &'static str, base_name: &str, expected_path: impl AsRef<Path>) -> Self {
        match IOFile::options().truncate(true).write(true).open(expected_path) {
            Err(err) => AssociatedIOFile::ErrorReading {
                associated_name,
                message: format!("couldn't open expected file (for writing): {}", err)
            },
            Ok(expected_file) => AssociatedIOFile::WriteExpected {
                associated_name,
                base_name: String::from(base_name),
                expected_file
            }
        }
    }

    #[allow(unused)]
    fn map<R>(self, fun: fn(T) -> R) -> AssociatedIOFile<R> {
        match self {
            AssociatedIOFile::ErrorReading { associated_name, message } => {
                AssociatedIOFile::ErrorReading { associated_name, message }
            },
            AssociatedIOFile::CheckExpected { associated_name, expected } => {
                AssociatedIOFile::CheckExpected { associated_name, expected: fun(expected) }
            },
            AssociatedIOFile::WriteExpected { associated_name, base_name, expected_file } => {
                AssociatedIOFile::WriteExpected { associated_name, base_name, expected_file }
            }
        }
    }

    #[allow(unused)]
    fn and_then<R, E: Error>(self, fun: fn(T) -> Result<R, E>) -> AssociatedIOFile<R> {
        match self {
            AssociatedIOFile::ErrorReading { associated_name, message } => {
                AssociatedIOFile::ErrorReading { associated_name, message }
            },
            AssociatedIOFile::CheckExpected { associated_name, expected } => match fun(expected) {
                Err(err) => AssociatedIOFile::ErrorReading {
                    associated_name,
                    message: format!("couldn't parse expected: {}", err)
                },
                Ok(expected) => AssociatedIOFile::CheckExpected { associated_name, expected }
            },
            AssociatedIOFile::WriteExpected { associated_name, base_name, expected_file } => {
                AssociatedIOFile::WriteExpected { associated_name, base_name, expected_file }
            }
        }
    }
}

impl<T: Eq + Display> AssociatedIOFile<T> {
    #[allow(unused)]
    fn check_or_replace(self, errors: &mut ErrorNodes, get_actual: impl FnOnce() -> T) {
        match self {
            AssociatedIOFile::ErrorReading { associated_name, message } => {
                error!(errors, "{}; {}", associated_name, message);
            },
            AssociatedIOFile::CheckExpected { associated_name, expected } => {
                let actual = get_actual();
                if actual != expected {
                    error!(errors, "expected {}, input mismatch\nexpected: <\n{}\n> but was <\n{}\n>", associated_name, expected, actual);
                }
            }
            AssociatedIOFile::WriteExpected { associated_name, base_name, mut expected_file } => {
                let actual = get_actual();
                let actual_display = actual.to_string();
                match expected_file.write(actual_display.as_bytes()) {
                    Err(err) => error!(errors, "failed to write actual {}: {}", associated_name, err),
                    Ok(_) => warn!("wrote actual {} for {} for regression", associated_name, base_name)
                }
            }
        }
    }
}

pub struct RunTestsOnFiles<T: 'static> {
    pub dir_name: &'static str,
    pub try_parse: fn(String, &Path) -> Result<T, Box<dyn Error>>,
    pub tests: &'static [RunTest<T>]
}

pub struct RunTest<T: 'static> {
    pub test_name: &'static str,
    /// Associated name and extension
    pub associated_files: &'static [(&'static str, &'static str)],
    pub run: fn(&mut ErrorNodes, &T, &Path, Vec<AssociatedIOFile<String>>)
}

impl<T: 'static> RunTestsOnFiles<T> {
    pub fn run(&self, errors: &mut ErrorNodes) {
        let mut dir_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        dir_path.push("tests/resources");
        dir_path.push(self.dir_name);

        // Ugly code
        let inputs = try_or_return!(
            dir_path.read_dir(),
            errors, "couldn't read serde inputs (readdir): {}", self.dir_name
        );
        for input in inputs {
            match input {
                Err(err) => error!(errors, "FAILED to open a serde input (readdir): {}", err),
                Ok(input) => errors.push_group(input.file_name().to_string_lossy(), |errors| {
                    self.test_file(errors, &dir_path, &input)
                })
            }
        }
    }

    fn test_file(&self, errors: &mut ErrorNodes, dir_path: &Path, input: &DirEntry) {
        // region load input boilerplate
        let file_path = input.path();
        if file_path.extension() != Some(OsStr::new("dui")) {
            info!("skipping non-dui file: {}", file_path.display());
            return;
        }
        let file_name = input.file_name();
        let base_name = match file_name.to_string_lossy() {
            Cow::Borrowed(name) => &name[..name.len() - 4],
            Cow::Owned(_) => "<not utf-8>"
        };
        let mut input = try_or_return!(
            IOFile::options().read(true).open(input.path()),
            errors, "couldn't open input"
        );
        let mut input_string = String::new();
        try_or_return!(
            input.read_to_string(&mut input_string),
            errors, "couldn't read input"
        );
        // endregion

        self.test_input(errors, dir_path, base_name, input_string, &file_path)
    }

    fn test_input(&self, errors: &mut ErrorNodes, dir_path: &Path, base_name: &str, input_string: String, input_path: &Path) {
        match (self.try_parse)(input_string, input_path) {
            Err(error) => error!(errors, "couldn't parse: {}", error),
            Ok(input) => {
                for test in self.tests {
                    errors.push_group(test.test_name, |errors| {
                        let associated_files = test.associated_files.iter().map(|(associated_name, extension)| {
                            self.load_associated(dir_path, base_name, associated_name, extension)
                        }).collect::<Vec<_>>();
                        (test.run)(errors, &input, input_path, associated_files)
                    });
                }
            }
        }

        /*
        // Test round trip
        let instance_str = (pretty_print)(&serial_instance)?;
        // Serializer's format is a bit different than ours
        // So we have to re-serialize to really check
        let instance2 = SerialDuiFile::try_from(&instance_str).map_err(|err| {
            error!("* failed on round-trip second deserialize");
            err
        })?;
        if instance2 != serial_instance {
            let instance_str2 = toml::to_string_pretty(&serial_instance)?;
            errors.push(SmallError(format!("round trip, input mismatch\nexpected: <\n{}\n> but was <\n{}\n> then was <\n{}\n> (ignore non-semantic differences)", input, instance_str, instance_str2).into()));
        }

        // Test finish serialization
        let instance = DuiFile::try_from(serial_instance)?;

        // Test expected debug
        expected_debug.check_or_replace(&mut errors, || format!("{:?}", instance));
        */
    }


    fn load_associated(&self, dir_path: &Path, base_name: &str, associated_name: &'static str, extension: &str) -> AssociatedIOFile<String> {
        let mut expected_path = PathBuf::from(dir_path);
        expected_path.push("expected_");
        let mut expected_path = expected_path.into_os_string();
        expected_path.push(associated_name);
        let mut expected_path = PathBuf::from(expected_path);
        expected_path.push(base_name);
        expected_path.set_extension(extension);

        if env::var("WRITE_REGRESSIONS").ok().is_some_and(|var| !var.is_empty() && var != "0" && var != "false") {
            AssociatedIOFile::write_expected(associated_name, base_name, expected_path)
        } else {
            match IOFile::options().read(true).open(&expected_path) {
                Err(err) => {
                    if err.kind() == std::io::ErrorKind::NotFound {
                        AssociatedIOFile::write_expected(associated_name, base_name, expected_path)
                    } else {
                        AssociatedIOFile::ErrorReading {
                            associated_name,
                            message: format!("couldn't open (for reading): {}", err)
                        }
                    }
                }
                Ok(mut file) => {
                    let mut expected_str = String::new();
                    if let Err(err) = file.read_to_string(&mut expected_str) {
                        AssociatedIOFile::ErrorReading {
                            associated_name,
                            message: format!("couldn't read: {}", err)
                        }
                    } else if expected_str.is_empty() {
                        AssociatedIOFile::write_expected(associated_name, base_name, expected_path)
                    } else {
                        AssociatedIOFile::CheckExpected { associated_name, expected: expected_str }
                    }
                }
            }
        }
    }
}