<!-- dash: #mermaid | Element | ###:Section -->



## Mermaid Element - #mermaid{}

  Mermaid is a Javascript library that generates diagram and flowchart from text.

### Usage

```erlang
	OptionsSequence = {sequence, [{showSequenceNumbers, true}, {width, 100}, {height, 100}]},
    Options = [{theme,"dark"}],
    Data = "sequenceDiagram\nAlice->John: Hello John, how are you?\n\nNote over Alice,John: A typical interaction",

    #mermaid{
		code=Data,
		options=Options,
		diagram_options=OptionsSequence
	}

```

### Attributes

* `code` (string) - The text which Mermaid will render.
* `options` (proplist) - A list of global Mermaid options using a standard Erlang
proplists (a list of 2-tuples, like `{Option, Value}`.
* `diagram_options` (`{DiagramType, Options}` tuple) - DiagramType can be `sequence`, `flowchat`, `gantt`. And Options is a list of Mermaid diagram options using a standard Erlang
proplists (a list of 2-tuples, like `{Option, Value}`.

### See Also

*  [base element](./element_base.md)
*  [Mermaid Flowchars and Diagrams](http://nitrogenproject.com/demos/demos_mermaid1)
*  [Mermaid with Postbacks](http://nitrogenproject.com/demos/demos_mermaid2)
*  [Mermaid Async Updates](http://nitrogenproject.com/demos/demos_mermaid3)
*  [Official Mermaid Documentation](https://mermaid-js.github.io/mermaid/#/)
