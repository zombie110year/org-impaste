use anyhow::Error;
use emacs::{defun, IntoLisp};
use reqwest::blocking::Response;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use sha2::{Digest, Sha256};
use std::str::FromStr;
use std::{io::Write, path::PathBuf};

const USER_AGENT: &'static str =
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:102.0) Gecko/20100101 Firefox/102.0";

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "org-impaste")]
fn init(_: &emacs::Env) -> emacs::Result<()> {
    Ok(())
}

/// download image from internet
///
/// + url : image's url.
/// + store : the directory to store image files.
/// + referer : set it if return 403 error without referer.
#[defun(name = "-download-external")]
fn download(
    env: &emacs::Env,
    url: String,
    store: String,
    referer: String,
) -> emacs::Result<emacs::Value<'_>> {
    let impath = download_(url, PathBuf::from(store), referer)?;
    return impath.into_lisp(&env);
}

fn download_(url: String, store: PathBuf, referer: String) -> Result<String, Error> {
    let im = if referer.is_empty() {
        fetch(&url, None)?
    } else {
        fetch(&url, Some(&referer))?
    }
    .bytes()?
    .to_vec();
    let filename = hex_filename(&im);
    let mime = tree_magic_mini::from_u8(&im[..]);
    let fileext = mime.split("/").last().unwrap();
    let impath = store.join(format!("{filename}.{fileext}"));
    let infopath = store.join(format!("{filename}.txt"));
    std::fs::write(&impath, im)?;
    let mut log = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&infopath)?;
    if referer.is_empty() {
        log.write(format!("{url}\n").as_bytes())?;
    } else {
        log.write(format!("{url}\t{referer}").as_bytes())?;
    }
    return Ok(impath.to_str().unwrap().to_string());
}

/// paste image from clipboard
#[defun(name = "-clipboard-external")]
fn clipboard(env: &emacs::Env) -> emacs::Result<emacs::Value<'_>> {
    let log = format!("org-impaste-clipboard");
    env.message(&log)
}

pub(crate) fn hex_filename(content: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content);
    let hash = hasher.finalize();
    let hexname = format!("{:x}", hash);
    return hexname;
}

// todo 多线程分片下载
pub(crate) fn fetch(url: &str, referer: Option<&str>) -> Result<Response, Error> {
    let headers = if let Some(referer) = referer {
        let mut headers = HeaderMap::with_capacity(2);
        headers.insert(
            HeaderName::from_str("referer")?,
            HeaderValue::from_str(&referer)?,
        );
        headers.insert(
            HeaderName::from_str("user-agent")?,
            HeaderValue::from_str(USER_AGENT)?,
        );
        headers
    } else {
        let mut headers = HeaderMap::with_capacity(1);
        headers.insert(
            HeaderName::from_str("user-agent")?,
            HeaderValue::from_str(USER_AGENT)?,
        );
        headers
    };
    let client = reqwest::blocking::ClientBuilder::default()
        .default_headers(headers)
        .build()?;
    let req = client.get(url).build()?;
    let resp = client.execute(req)?;
    Ok(resp)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_download() {
        let x = download_(
            "https://o.acgpic.net/img-original/img/2022/06/22/00/04/00/99213747_p0.jpg".to_string(),
            PathBuf::new().join("debug"),
            "https://pixivic.com/".to_string(),
        )
        .unwrap();
        println!("{x}");
    }
}
