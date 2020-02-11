with Tau.Types.Standard;

package body Tau.Environment is

   Local_Standard_Library   : Tau_Environment;
   Local_Global_Environment : Tau_Environment;

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
     (Environment : not null access Root_Tau_Environment'Class;
      Name        : String)
      return Tau_Environment
   is
      Parent : constant Tau_Environment := Tau_Environment (Environment);
   begin
      return Child : constant Tau_Environment := new Root_Tau_Environment'
        (Tau.Objects.Root_Tau_Object with
           Parent      => Parent,
         Map         => <>,
         Env_Type    => null,
         Errors      => <>,
         Error_Stack => <>,
         Children    => <>)
      do
         Child.Initialize_Object (GCS.Positions.Null_Position, Name);
         Environment.Children.Append (Child);
      end return;
   end Create_Child;

   -----------------------------
   -- Create_Standard_Library --
   -----------------------------

   procedure Create_Standard_Library is
      Env : Tau_Environment renames Local_Standard_Library;

      procedure Insert (T : Tau.Types.Tau_Type);

      ------------
      -- Insert --
      ------------

      procedure Insert (T : Tau.Types.Tau_Type) is
      begin
         Env.Insert
           (T.Name,
            Tau.Entries.Type_Entry
              (GCS.Positions.Null_Position, T.Name, T));
      end Insert;

   begin

      Env :=
        new Root_Tau_Environment'
          (Tau.Objects.Root_Tau_Object with
             Parent      => null,
           Map         => <>,
           Env_Type    => null,
           Errors      => <>,
           Error_Stack => <>,
           Children    => <>);
      Env.Initialize_Object (GCS.Positions.Null_Position, "standard");

      Insert (Tau.Types.Standard.Tau_Integer);
      Insert (Tau.Types.Standard.Tau_Float);
      Insert (Tau.Types.Standard.Tau_Vector (2));
      Insert (Tau.Types.Standard.Tau_Vector (3));
      Insert (Tau.Types.Standard.Tau_Vector (4));
      Insert (Tau.Types.Standard.Tau_Matrix (2, 2));
      Insert (Tau.Types.Standard.Tau_Matrix (3, 3));
      Insert (Tau.Types.Standard.Tau_Matrix (4, 4));

      Local_Global_Environment := Env.Create_Child ("tau");

   end Create_Standard_Library;

   ------------------------
   -- Global_Environment --
   ------------------------

   function Global_Environment return Tau_Environment is
   begin
      return Local_Global_Environment;
   end Global_Environment;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Environment : in out Root_Tau_Environment;
      Name        : String;
      Item        : not null access Tau.Entries.Root_Tau_Entry'Class)
   is
   begin
      Environment.Map.Insert (Name, Tau.Entries.Tau_Entry (Item));
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Environment : in out Root_Tau_Environment'Class;
      Name        : String;
      Value       : not null access
        Tau.Values.Root_Tau_Value'Class)
   is
   begin
      Environment.Insert
        (Name,
         Tau.Entries.Value_Entry
           (Value.Defined_At, Name, Tau.Values.Tau_Value (Value)));
   end Insert;

--     --------------------
--     -- Iterate_Errors --
--     --------------------
--
--     procedure Iterate_Errors
--       (Environment : Root_Tau_Environment;
--        Process     : not null access procedure
--          (Position : GCS.Positions.File_Position;
--           Message : String))
--     is
--        All_Errors : Error_Lists.List;
--
--        procedure Collect (Env : Root_Tau_Environment'Class);
--
--        -------------
--        -- Collect --
--        -------------
--
--        procedure Collect (Env : Root_Tau_Environment'Class) is
--        begin
--           for Error of Env.Errors loop
--              All_Errors.Append (Error);
--           end loop;
--           for Child of Env.Children loop
--              Collect (Child.all);
--           end loop;
--        end Collect;
--
--     begin
--        Collect (Environment);
--        Error_Sorting.Sort (All_Errors);
--        for Error of All_Errors loop
--           Process (Error.Position, Error.Message);
--        end loop;
--     end Iterate_Errors;

   ---------------------
   -- Set_Return_Type --
   ---------------------

   procedure Set_Return_Type
     (Environment : in out Root_Tau_Environment'Class;
      Return_Type : Tau.Types.Tau_Type)
   is
   begin
      Environment.Env_Type := Return_Type;
   end Set_Return_Type;

   ----------------------
   -- Standard_Library --
   ----------------------

   function Standard_Library return Tau_Environment is
   begin
      if Local_Standard_Library = null then
         Create_Standard_Library;
      end if;
      return Local_Standard_Library;
   end Standard_Library;

   ------------------
   -- Write_Errors --
   ------------------

--     procedure Write_Errors
--       (Environment : Root_Tau_Environment)
--     is
--
--        procedure Put (Position : GCS.Positions.File_Position;
--                       Message  : String);
--
--        ---------
--        -- Put --
--        ---------
--
--        procedure Put (Position : GCS.Positions.File_Position;
--                       Message  : String)
--        is
--        begin
--           Ada.Text_IO.Put_Line
--             (Ada.Text_IO.Standard_Error,
--              GCS.Positions.Image (Position)
--              & ": " & Message);
--        end Put;
--
--     begin
--        Environment.Iterate_Errors (Put'Access);
--     end Write_Errors;

end Tau.Environment;
