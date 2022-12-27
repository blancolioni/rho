with Ada.Strings.Fixed;

package body Rho.Shaders.Partial is

   ------------
   -- Append --
   ------------

   procedure Append
     (Container : in out Partial_Shader_Container;
      Partial   :        Partial_Shader_Type)
   is
   begin
      if Partial /= null then
         Container.List.Append (Partial);
      end if;
   end Append;

   -------------
   -- Iterate --
   -------------

   overriding procedure Iterate
     (Container : Partial_Shader_Container;
      Stage     : Shader_Stage;
      Process   : not null access procedure (Partial : Partial_Shader_Type))
   is
   begin
      for Partial of Container.List loop
         if Partial.Stage = Stage then
            Process (Partial);
         end if;
      end loop;
   end Iterate;

   -------------------
   -- Iterate_Lines --
   -------------------

   procedure Iterate_Lines
     (This        : Root_Partial_Shader_Type;
      Environment : Partial_Shader_Environment'Class;
      Process     : not null access
        procedure (Line : String))
   is
      Global_Context : String_Lists.List;

      function To_Context (Text : String) return String_Lists.List;
      function Check_Context (Context : String_Lists.List) return Boolean;

      procedure Process_Line
        (Line : String);

      -------------------
      -- Check_Context --
      -------------------

      function Check_Context (Context : String_Lists.List) return Boolean is
      begin
         for Name of Context loop
            if Name'Length > 3
              and then Name (Name'First .. Name'First + 2) = "no-"
            then
               if Environment.Check (Name (Name'First + 3 .. Name'Last)) then
                  return False;
               end if;
            else
               if not Environment.Check (Name) then
                  return False;
               end if;
            end if;
         end loop;
         return True;
      end Check_Context;

      ------------------
      -- Process_Line --
      ------------------

      procedure Process_Line
        (Line : String)
      is
      begin
         if Line = "" then
            return;
         end if;
         if Line (Line'First) = '['
           and then Line (Line'Last) = ']'
         then
            Global_Context :=
              To_Context (Line (Line'First + 1 .. Line'Last - 1));
         else
            if Check_Context (Global_Context) then
               declare
                  Colon_Index : constant Natural :=
                                  Ada.Strings.Fixed.Index (Line, ":");
               begin
                  if Colon_Index = 0 then
                     Process (Line);
                  else
                     if Check_Context
                       (To_Context (Line (Line'First .. Colon_Index - 1)))
                     then
                        Process (Line (Colon_Index + 1 .. Line'Last));
                     end if;
                  end if;
               end;
            end if;
         end if;
      end Process_Line;

      ----------------
      -- To_Context --
      ----------------

      function To_Context (Text : String) return String_Lists.List is
         Start_Index : Positive := Text'First;
      begin
         return Context : String_Lists.List do
            for I in Text'Range loop
               if Text (I) = ',' then
                  declare
                     Name : constant String :=
                              Text (Start_Index .. I - 1);
                  begin
                     if Name /= "" then
                        Context.Append (Name);
                     end if;
                     Start_Index := I + 1;
                  end;
               end if;
            end loop;

            if Start_Index < Text'Last then
               Context.Append (Text (Start_Index .. Text'Last));
            end if;
         end return;
      end To_Context;

   begin
      for Line of This.Lines loop
         Process_Line (Line);
      end loop;
   end Iterate_Lines;

end Rho.Shaders.Partial;
