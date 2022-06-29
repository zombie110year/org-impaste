use anyhow::Error;
use arboard::Clipboard;
use image::EncodableLayout;
use std::io::{BufWriter, Cursor, Write};
use std::path::PathBuf;

use crate::prelude::*;

pub(crate) fn clipboard(store: PathBuf) -> Result<String, Error> {
    let (height, width, raw) = copied_image()?;
    let png = build_png(height as u32, width as u32, raw)?;
    let filename = hex_filename(&png);
    let impath = store.join(format!("{filename}.png"));
    std::fs::write(&impath, &png)?;
    return Ok(impath.to_str().unwrap().to_string());
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
