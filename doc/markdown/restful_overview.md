# Nitrogen Restful Elements

## Overview

  Restful elements introduce the possibility to create forms that don't
  depend on AJAX/Javascript. This is sometimes useful, for example if you
  want to provide a fallback option when a client does not support
  javascript or sits behind a firewall that filters active content.

## Demo

  Check out the [RESTful Forms Demo](http://nitrogenproject.com/demos/restful)
  
## How it works

  Restful requests work a little different compared to the rest of
  Nitrogen. They don't use the event mechanism. Instead you have to
  render the reply either on a different page or check if
  wf:q(restful_method) is set to "post" or "get" depending on the
  method of the form.  Form elements that don't rely on javascript are
  useable in restful_form elements. The elements that are supported are
  described below.

  Each control within a #restful_form{} element will submit its Nitrogen
  id as the name of the field, unless overridden by specifying an html_name
  on the restful elements.

## Replaced and new elements

### New Elements

 *  [restful_form](restful_form.md)

### Replaced elements
   Some elements are replaced with restful_ counterparts:


 *  [restful_submit](restful_submit.md) and [restful_reset](restful_reset.md) instead of [button](button.md) 
 *  [restful_upload](./restful_upload) instead of upload

## Supported elements

 *  [Textbox](textbox.md)
 *  [Textarea](textarea.md)
 *  [Checkbox](checkbox.md)
 *  [Dropdown](dropdown.md)
 *  [Password Box](password.md)
 *  [Hidden Value](hidden.md)
 *  [Radio Button](radio.md)

## No support for validators
  Validators are not currently supported for restful_submit actions, so be sure extra sure to check your inputs.

## Example
```erlang
  case wf:q(restful_method) of
    "post" -> [
        % this is code is executed after the submit button was pressed

        #table{rows=[
            #tablerow{cells=[
                #tablecell{text="text_input"},
                #tablecell{text=wf:q(text_input)}
            ]}
         ]};
    _else -> [
        % this code is executed when restful_method is "get" 
        % or undefined

        #restful_form{id=restful_form, method=post, body=[
           #table{rows=[
                #tablerow{cells=[
                    #tablecell{text="text_input"},
                    #tablecell{body=[#textbox{id=text_input}]}
               ]}
            ]},
            #restful_reset{},
            #restful_submit{}
         ]}
    ]
  end

```
