<!-- dash: #bind | Element | ###:Section -->



## Bind Element - #bind {}

  The bind element allows you to easily display lists of data on a Nitrogen
  page. When you declare the bind element, you define a set of 'data', a 'body'
  containing one or more elements, and a 'map' that tells the bind element how
  to map a data row into the body.

  A map item is an atom of the form `element_id@attribute`. The `element_id`
  must correspond to the id of an element in the 'body', and the attribute must
  be an attribute found on that element. You can bind to any attribute within
  an element.

  In addition, you can specify a 'transform' function to manipulate the data at
  bind time and add additional bindings dynamically.

  The 'data' attribute and the 'map' attribute must follow the same basic
  structure. This can mean one of three things:

 *  Simple List :: If the data is a simple list, then each row must be of the
     same length, and the map must also be the same length as a data row.

 *  Value Pair :: If the data is in the form of a key/value pair, then each row
     must be a list of {key, value} tuples, and the map must also be a list of
     {key, value} tuples.  The tuples can be in any order, and there can be extra
     tuples or missing tuples with no ill effects.

 *  Records :: If the data is in the form of a record, then each row must be
     the same type of record, and the map must be a record.  

  The bind element can be nested within itself.

### Usage

```erlang

   Data = [
     ["Joe", "Franklin"],
     ["Samir", "Jahal"],
     ["Laurie", "O'Conner"]
   ],

   Map = [firstName@text, lastName@text],

   #bind { data=Data, map=Map, body=[
     #label { id=firstName },
     #label { id=lastName }
   ]}

```

### Attributes

   * `data` (list of erlang terms) - Data in the form of a simple list, a key/value pair, records, or any combination of the three.

   * `map` (erlang terms) - Specify how the data maps to the body of elements.

   * `transform` (Function(DataRow, Acc) -> {DataRow1, Acc1, ExtraBindings}) - 
   A transform function that is run on the data before binding. DataRow contains 
   one row of data. Acc contains the accumulator set by the 'acc' attribute. 
   The function can return a modified datarow, an updated accumulator, 
   and a list of extra bindings to apply. The extra bindings must be of the
   form `{element_id@attr, value}`.

   * `acc` (Erlang term) - The initial accumulator to pass in to the transform function.

   * `body` (Nitrogen elements) - The block of Nitrogen elements to which data is bound.

   * `empty_body` (Nitrogen elements) - A block of Nitrogen elements to render if the supplied data has no rows.

### See Also

 *  [base element](./element_base.md)

 *  [Data Binding Section of the Demos page](http://nitrogenproject.com/demos)
