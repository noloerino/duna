//! Contains type aliases and structs used across the lex/parse/assemble/link chain.
use std::fmt;

/// The line number of a token.
pub type LineNo = usize;
/// The offset of a token within a line.
pub type LineOffs = usize;

/// The location of a token or a character.
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Hash, Debug)]
pub struct Location {
    /// Holds a reference to the file from which this came.
    pub file_id: FileId,
    /// Holds a reference to the line from which this came (used for error messages)
    pub lineno: LineNo,
    pub offs: LineOffs,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.lineno, self.offs)
    }
}

/// The name and contents of a file.
pub struct FileData {
    pub file_name: String,
    pub content: String,
}

impl FileData {
    #[cfg(test)]
    pub fn from_test_program(content: &str) -> FileData {
        FileData {
            file_name: "test".to_string(),
            content: content.to_string(),
        }
    }
}

/// Used to make error reporting easier without having to worry about lifetimes with string
/// references.
pub type FileId = usize;
/// Maps a FileId to the corresponding FileData.
pub type FileMap = Vec<FileData>;
