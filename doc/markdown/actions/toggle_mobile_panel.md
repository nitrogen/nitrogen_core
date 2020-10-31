

## Toggle Mobile Panel Action - #toggle_mobile_panel {}

  This action will toggle a [Mobile Panel](mobile_panel.html)
  (collapsible mobile side menu typicaly of mobile applications).

### Usage

```erlang
   wf:wire(close_button, panel_id, #toggle_mobile_panel{}).

	%% OR %%

	wf:wire(close_button, #toggle_mobile_panel{ target=panel_id}).

```

### See Also

 *  [base element](./base.html)

	*  [Mobile Panel Element](mobile_panel.html)

	*  [Mobile Panel Demonstration](/demos/mobile_panel)

	*  [Toggle Action](toggle.html) (non-mobile version)
