## Before Postback Action - #before_postback {}

Adds custom javascript to be executed before every postback is performed on the
page.

### Usage

```erlang
	wf:wire(#before_postback{script="console.log('Sending a postback');"}).
```

### Attributes

* `script`: Raw JavaScript to be executed before postback is sent.

### Notes

The provided script will literally run before every postback on the page.

### See Also

* [base action](./action_base.md)
* [js_fun action](./js_fun.md)
* [script action](./script.md)

