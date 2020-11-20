<!-- dash: Base Action | Event |  -->



## Base Action

  In object-oriented parlance, all Nitrogen actions are subclasses of the base
  actions. This means that all Nitrogen elements can use the attributes listed
  below.

### Attributes

   * `module` (atom) - The module containing the logic for this action. Set 
    automatically.

   * `trigger` (atom) - The id of the Nitrogen element that will trigger
    this action. Set automatically.

   * `target` (atom) - The id of the Nitrogen element to be referenced by
    `obj('me')`. Set automatically.

   * `actions` (list af actions) - A list of actions grouped within this
    action.

   * `show_if` (boolean) - If set to true, this action will be rendered.
    Otherwise, it will not.

   * `dependency_js` (URL string) - If this attribute is set, before this
    script is run, the specified URL script will be downloaded and executed.
    This is useful when an action or element will not function without first
    downloading a necessary jQuery plugin or otherwise to the page, but it saves
    in bandwidth since it doesn't download the dependency until it's needed.
