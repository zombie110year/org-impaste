use anyhow::Error;
use sha2::{Digest, Sha256};

/// Read the byte array's content, calculate the sha256 hash, then format it to hex string.
pub(crate) fn hex_filename(content: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content);
    let hash = hasher.finalize();
    let hexname = format!("{:x}", hash);
    return hexname;
}

/// Get the nanoseconds from 1970.1.1, and format it to hex string.
pub(crate) fn timer_hex() -> Result<String, Error> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let now = SystemTime::now();
    let seed = now.duration_since(SystemTime::from(UNIX_EPOCH))?.as_nanos();
    Ok(format!("{:x}", seed))
}
