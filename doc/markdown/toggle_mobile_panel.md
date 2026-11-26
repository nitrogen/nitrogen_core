<!-- dash: #toggle_mobile_panel | Event | ###:Section -->



## Toggle Mobile Panel Action - #toggle_mobile_panel {}

  This action will toggle a [Mobile Panel](mobile_panel.md)
  (collapsible mobile side menu typicaly of mobile applications).

### Usage

```erlang
   wf:wire(close_button, panel_id, #toggle_mobile_panel{}).

	%% OR %%

	wf:wire(close_button, #toggle_mobile_panel{ target=panel_id}).

```

### See Also

 *  [base element](./action_base.md)

 *  [Mobile Panel Element](mobile_panel.md)

 *  [Mobile Panel Demonstration](https://nitrogenproject.com/demos/mobile_panel)

 *  [Toggle Action](toggle.md) (non-mobile version)
