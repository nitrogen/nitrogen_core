

## API Action - #api {}

  This action will wire an API function to the page for easily initiating 
  postbacks through javascript. These postbacks will be expecting 
  the `api_event(ApiName,Tag,JSArgs)` to be defined in the module.

### Usage

```erlang
   wf:wire(#api { name=myApi, tag=some_term})

```

### Attributes

   * `name` (atom) - The name of the function to be created. In the example above, the function to call will be `page.myApi(Args)`

   * `tag` (Erlang term) - Any erlang term to be the tag of the postback. This value is not exposed to the client, and so it can be used for transmitting something you wouldn't want the client to see, like Erlang Pids, or LoginIDs, for example.

   * `delegate` (atom) - The module to handle the postback events. If unspecified, will default to the current page.

### Postback Function

 *  api_event(ApiName,Tag,JSArgs) :: This is the function that will receive the postbacks. `ApiName` and `Tag` correspond to the `name` and `tag` attributes of the #api{} element. JSArgs is a list of the arguments passed to the javascript function.  JSON Objects will be re-encoded to an Erlang proplist.

### Example

```erlang
   %% Wiring the API to the page
   wf:wire(#api { name=hello_english, tag="English" }),
   wf:wire(#api { name=hello_german, tag="German" }).

```

```javascript
   // Let's call the API in javascript
   page.hello_english();
   page.hello_german("Guten Tag!");

```

```erlang
   %% Let's handle the events now. The tag will become the language and first Argument, if specified will become an override value.
   api_event(API, Lang, Args)
     when API==hello_english;API==hello_german ->
       Msg = say_hello(Lang,Args),
       wf:flash(Lang ++ " speaker says " ++ Msg).

   say_hello(english,[]) ->
     "Hello";
   say_hello(german,[]) ->
     "Hallo";
   say_hello(_,[Override | _ ]) ->
     Override.

```

 *  The result of calling above will generate two flash messages reading:

 *  English speaker says Hello

 *  German speaker says Guten Tag!

### See Also

 *  [base element](./action_base.md)

 *  [confirm element](./confirm.md)

 
