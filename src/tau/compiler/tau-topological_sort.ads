with Ada.Containers.Doubly_Linked_Lists;

generic
   type Element_Type is private;
   with function Is_Dependent_On
     (Element, Dependency : Element_Type)
      return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   with package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type);
package Tau.Topological_Sort is

   procedure Sort (List : in out Element_Lists.List);

end Tau.Topological_Sort;
