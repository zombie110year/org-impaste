use anyhow::Error;
use arboard::Clipboard;
use emacs::{defun, IntoLisp};
use image::EncodableLayout;
use reqwest::blocking::Response;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use sha2::{Digest, Sha256};
use std::io::{BufWriter, Cursor, Write};
use std::path::PathBuf;
use std::str::FromStr;

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
fn clipboard(env: &emacs::Env, store: String) -> emacs::Result<emacs::Value<'_>> {
    let impath = clipboard_(PathBuf::from(store))?;
    return impath.into_lisp(env);
}

pub(crate) fn clipboard_(store: PathBuf) -> Result<String, Error> {
    let (height, width, raw) = copied_image()?;
    let png = build_png(height as u32, width as u32, raw)?;
    let filename = hex_filename(&png);
    let impath = store.join(format!("{filename}.png"));
    std::fs::write(&impath, &png)?;
    return Ok(impath.to_str().unwrap().to_string());
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

/// return (height, width, bytes)
pub(crate) fn copied_image() -> Result<(usize, usize, Vec<u8>), Error> {
    let mut ctx = Clipboard::new()?;
    let im = ctx.get_image()?;
    let width = im.width;
    let height = im.height;
    let raw = im.bytes;
    let channels = raw.len() / (width * height);
    let raw = if channels == 3 {
        raw.to_vec()
    } else if channels > 3 {
        let mut buf = Vec::with_capacity(width * height * 3);
        for chunk in raw.chunks(channels) {
            buf.write(&chunk[..3])?;
        }
        buf
    } else {
        std::fs::write("debug.org-impaste.bin", raw.as_bytes())?;
        return Err(anyhow::anyhow!(
            "what image has channels {channels}?, debug info saved 'debug.org-impaste.bin'."
        ));
    };
    return Ok((height, width, raw));
}

pub(crate) fn build_png(height: u32, width: u32, content: Vec<u8>) -> Result<Vec<u8>, Error> {
    if let Some(raw) = image::RgbImage::from_raw(width as u32, height as u32, content) {
        let mut imfile = Cursor::new(Vec::<u8>::with_capacity((height * width * 3) as usize));
        {
            let mut imbuf = BufWriter::new(&mut imfile);
            raw.write_to(&mut imbuf, image::ImageOutputFormat::Png)?;
        }
        let im = &imfile.get_ref()[..];
        return Ok(im.to_vec());
    } else {
        return Err(anyhow::anyhow!(
            "ImageData's pixels are not enough to build {height}x{width} image"
        ));
    }
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

    #[test]
    fn test_clipboard() {
        let x = clipboard_(PathBuf::from("debug")).unwrap();
        println!("{x}");
    }
}
