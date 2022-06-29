use anyhow::Error;
use sha2::{Digest, Sha256};

pub(crate) fn hex_filename(content: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content);
    let hash = hasher.finalize();
    let hexname = format!("{:x}", hash);
    return hexname;
}

pub(crate) fn timer_key() -> Result<String, Error> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let now = SystemTime::now();
    let seed = now.duration_since(SystemTime::from(UNIX_EPOCH))?.as_nanos();
    Ok(format!("{:x}", seed))
}
