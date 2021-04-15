package body Tau.Topological_Sort is

   ----------
   -- Sort --
   ----------

   procedure Sort (List : in out Element_Lists.List) is

      type Graph_Vertex is
         record
            Node : Element_Type;
            Deps : Element_Lists.List;
         end record;

      package Element_Graphs is
        new Ada.Containers.Doubly_Linked_Lists (Graph_Vertex);

      Graph  : Element_Graphs.List;
      Result : Element_Lists.List;
      Pool   : Element_Lists.List;

      function Find_Node
        (Element : Element_Type)
         return Element_Graphs.Cursor
        with Unreferenced;

      ---------------
      -- Find_Node --
      ---------------

      function Find_Node
        (Element : Element_Type)
         return Element_Graphs.Cursor
      is
      begin
         for Position in Graph.Iterate loop
            if Element_Graphs.Element (Position).Node = Element then
               return Position;
            end if;
         end loop;
         return Element_Graphs.No_Element;
      end Find_Node;

   begin

      if Natural (List.Length) <= 1 then
         return;
      end if;

      for Check_Element of List loop
         declare
            Deps  : Element_Lists.List;
         begin
            for Dep_Element of List loop
               if Check_Element /= Dep_Element
                 and then Is_Dependent_On (Check_Element, Dep_Element)
               then
                  Deps.Append (Dep_Element);
               end if;
            end loop;

            Graph.Append ((Check_Element, Deps));

            if Deps.Is_Empty then
               Pool.Append (Check_Element);
            end if;
         end;
      end loop;

      if Pool.Is_Empty then
         raise Constraint_Error with
           "dependency graph is not acyclic";
      end if;

      while not Pool.Is_Empty loop
         declare
            N        : constant Element_Type := Pool.First_Element;
         begin
            Pool.Delete_First;
            Result.Append (N);

            for Vertex of Graph loop
               declare
                  Position : Element_Lists.Cursor := Vertex.Deps.Find (N);
               begin
                  if Element_Lists.Has_Element (Position) then
                     Vertex.Deps.Delete (Position);
                     if Vertex.Deps.Is_Empty then
                        Pool.Append (Vertex.Node);
                     end if;
                  end if;
               end;
            end loop;
         end;
      end loop;

      List := Result;

   end Sort;

end Tau.Topological_Sort;
