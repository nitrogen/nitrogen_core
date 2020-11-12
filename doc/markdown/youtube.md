

## Youtube Element - #youtube {}

  The Youtube element produces an Youtube embedded video.

### Usage

```erlang
   #youtube{
	   key="nlV4gm8SpVA",
	   allowfullscreen=true
	}

```

### Attributes

   * `key` (string) - The Youtube identifier of the video, or the URL to the
	video. Valid examples are: "nlV4gm8SpVA", "http://youtu.be/nlV4gm8SpVA",
	"https://www.youtube.com/watch?v=nlV4gm8SpVA", etc..

   * `width` (integer) - Width of the video tag.

   * `height` (integer) - Height of the video tag.

   * `allowfullscreen` (boolean) - If set to `true`, fullscreen mode will be allowed.

### See Also

 *  [base element](./element_base.md)

 *  [video element](./video.md)
