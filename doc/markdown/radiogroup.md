<!-- dash: #radio_group | Element | ###:Section -->


## Radio Group Element - #radio_group {}

  The radio group element automatically groups it's radio button members into the same radio button group by applying the same HTML `name` to each radio contained within.

Note: As of Nitrogen 2.4, this will do a "deep dive" in the body elements.

### Usage

```erlang
   #radiogroup { id=fruit, body= [
      #radio { text="Apple", value="apple", checked=true },
      #radio { text="Orange", value="orange" },
      #radio { text="Banana", value="banana" }
   ]}.

```

### Attributes

   * `body` (string) - The contents of the radio group. `#radio` elements contained within will be found and have the HTML `name` attributes set appropriately.

### See Also

 *  [base](./element_base.md)

 *  [radio](./radio.md)

 *  [Button](./button.md)

 *  [Link](./link.md)

 *  [Textbox](./textbox.md)

 *  [Password](./password.md)

 *  [Textarea](./textarea.md)

 *  [Dropdown](./dropdown.md)

 *  [Dropdown Option](./option.md)

 *  [RESTful element overview](./restful_overview.md)

 *  [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)

 *  [Radio Buttons Demo](http://nitrogenproject.com/demos/radio)
