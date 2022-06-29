use crate::prelude::*;
use anyhow::Error;
use reqwest::blocking::Response;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use std::io::Write;
use std::path::PathBuf;
use std::str::FromStr;

pub(crate) fn download(url: String, store: PathBuf, referer: String) -> Result<String, Error> {
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
        log.write(format!("{url}\t{referer}\n").as_bytes())?;
    }
    return Ok(impath.to_str().unwrap().to_string());
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
