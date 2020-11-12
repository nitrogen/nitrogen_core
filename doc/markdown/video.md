

## Video Element - #video {}

  The video element produces an HTML video tag.

### Usage

```erlang
   #video{
		src="/my/path/video.mp4",
		poster="/path/to/preview.png",
		controls=true,
		autoplay=false
	}

```

### Attributes

   * `src` (string) - URL of the video contents. Corresponds to the `src`
	  attribute in the `<video>` tag.

   * `poster` (string) - URL of the "preview" image.

   * `width` (integer) - Width of the video tag.

   * `height` (integer) - Height of the video tag.

   * `loop` (boolean) - If set to `true`, the video will loop automatically.

   * `muted` (boolean) - If set to `true`, the video will be silent.

   * `autoplay` (boolean) - If set to `true`, the video will play automatically.

   * `controls` (boolean) - If set to `true`, the video will include the
	  standard controls (start, stop, etc).

   * `body_no_support` (HTML or Nitrogen elements) - If the `<video>` tag is
	  not supported by the browser, display this instead.

   * `preload` ('auto' | 'metadata' | 'none') - If set to the atom `auto`, the browser will download the whole video when the page loads. If set to the atom `metadata`, the browser will download just the metadata of the video when the page loads. And if set to the atom `none`, the browser will not load the video at all when the page loads.

### See Also

 *  [base element](./element_base.md)
