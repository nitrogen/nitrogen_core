<!-- dash: #console_log | Event | ###:Section -->


## Console Log Action - #console_log {}

  This performs a safe `console.log()` call to print something some text or Erlang terms to your browser's javascript console. If your browser doesn't support `console.log()`, the call is ignored.

### Usage

```erlang
	%% Sending text
	wf:wire(#console_log { text="Some text" }),

	%% Sending erlang terms
	wf:wire(#console_log { text={some,random,<<"Erlang">>,"Terms"} }),

	%% Convenience method
	wf:console_log("Some text to send to the console"),

```

### Attributes

   * `text` (string/ or /Erlang term) - If text is a string, it'll simply
	  print the text to the console. If it's an Erlang term, the terms will be
	  converted to a string and sent to the browser.

### See Also

 *  [base element](./action_base.md)

 *  [alert element](./alert.md)

 
