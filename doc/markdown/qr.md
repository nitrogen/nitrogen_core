

## QR Code Element - #qr {}

  The QR element produces a QR code as an image on the page. Note: This element
  uses the Google Charts API to contstruct the QR code.

### Usage

```erlang
   #qr {data="http://nitrogenproject.com"}

```

### Attributes

   * `size` (integer) - How many pixels to make the image. (Default: 200)

   * `data` (string) - Whatever data you wish to encode in the QR code. If a
	  URL is specified, then it will be a URL QR code.  If left blank, the URL of
	  the current page will be used.

### See Also

 *  [base element](./element_base.md)
 *  [image element](./image.md)
	*  [QR Code Demos](/demos/qr)
