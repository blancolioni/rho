package body Tau.Types.Textures is

   type Tau_Texture_Type is
     new Root_Tau_Type with
      record
         Order : Positive;
      end record;

   Local_Textures : array (1 .. 3) of Tau_Type;
   Have_Local_Textures : Boolean := False;

   procedure Check_Local_Textures;

   -------------------------
   -- Check_Local_Textures --
   -------------------------

   procedure Check_Local_Textures is
   begin
      if not Have_Local_Textures then
         for I in Local_Textures'Range loop
            declare
               T : Tau_Texture_Type :=
                 Tau_Texture_Type'
                   (Root_Tau_Type with
                    Order => I);
            begin
               T.Initialize_Object
                 (GCS.Positions.Null_Position,
                  "texture_" & Character'Val (Character'Pos ('0') + I) & "D");
               Local_Textures (I) := new Tau_Texture_Type'(T);
            end;
         end loop;
         Have_Local_Textures := True;
      end if;
   end Check_Local_Textures;

   -------------
   -- Texture --
   -------------

   function Texture (Order : Positive) return Tau_Type is
   begin
      Check_Local_Textures;
      return Local_Textures (Order);
   end Texture;

end Tau.Types.Textures;
