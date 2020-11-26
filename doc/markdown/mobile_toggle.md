<!-- dash: #mobile_toggle | Element | ###:Section -->


## Mobile Toggle Element - #mobile_toggle {}

This element produces a [toggle switch](http://jquerymobile.com/demos/1.1.0/docs/forms/switch/index.html) formatted by jQuery Mobile.

### Usage

```erlang
   #mobile_toggle { 
      id=room_lights,
      theme=c,
      on_text="Lights On",
      on_value="on",
      off_text="Lights Off",
      off_value="off,
      selected="off",
      postback=switch_lights
   }

```

### Attributes
 
   * `on_text` (string) - The text label of the 'on' position (default "On")

   * `off_text` (string) - The text label of the 'off' position (default "Off")

   * `on_value` (string) - The value that is submitted with postbacks when in the 'on' position (default "on")

   * `off_value` (string) - The value that is submitted with postbacks when in the 'off' position (default "off")

   * `selected` (string) - The default selected value (default "off")

   * `postback` (term) - If specified, will postback immediately on hitting the toggle

### See Also

 *  [jQuery mobile elements](./jquery_mobile.md)

 *  [base element](./element_base.md)

 *  [dropdown element](./dropdown.md)
