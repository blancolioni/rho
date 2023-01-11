private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Strings.Unbounded;

private with WL.String_Maps;

package Rho.Signals is

   type Signal_Type is private;

   function Signal_Name (Signal : Signal_Type) return String;

   function New_Signal (Name : String) return Signal_Type;

   type Handler_Id is private;
   Null_Handler_Id : constant Handler_Id;

   type Signal_Data_Interface is interface;

   function No_Signal_Data return Signal_Data_Interface'Class;

   type Signal_Object_Interface is limited interface;

   procedure Emit_Signal
     (Object   : Signal_Object_Interface;
      Signal   : Signal_Type;
      Data     : Signal_Data_Interface'Class)
   is abstract;

   type Handler_Result is (Propagate, Stop);

   type Handler_Type is access
     function (Object      : not null access Signal_Object_Interface'Class;
               Signal_Data : Signal_Data_Interface'Class;
               User_Data   : Signal_Data_Interface'Class)
               return Handler_Result;

   function Add_Handler
     (Object   : in out Signal_Object_Interface;
      Signal   : Signal_Type;
      Handler  : Handler_Type;
      Data     : Signal_Data_Interface'Class)
      return Handler_Id
      is abstract;

   procedure Remove_Handler
     (Object   : in out Signal_Object_Interface;
      Signal   : Signal_Type;
      Id       : Handler_Id)
   is abstract;

   type Signal_Dispatcher is tagged private;

   procedure Initialize
     (This   : in out Signal_Dispatcher'Class;
      Object : not null access Signal_Object_Interface'Class);

   procedure Emit_Signal
     (This     : Signal_Dispatcher'Class;
      Signal   : Signal_Type;
      Data     : Signal_Data_Interface'Class);

   function Add_Handler
     (This     : in out Signal_Dispatcher'Class;
      Signal   : Signal_Type;
      Handler  : Handler_Type;
      Data     : Signal_Data_Interface'Class)
      return Handler_Id;

   procedure Remove_Handler
     (This     : in out Signal_Dispatcher'Class;
      Signal   : Signal_Type;
      Id       : Handler_Id);

private

   type Signal_Type is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Handler_Id is new Natural;

   Null_Handler_Id : constant Handler_Id := 0;

   package Data_Holder is
     new Ada.Containers.Indefinite_Holders
       (Signal_Data_Interface'Class);

   type Signal_Object_Access is access all Signal_Object_Interface'Class;

   type Handler_Record is
      record
         Id      : Handler_Id;
         Handler : Handler_Type;
         Data    : Data_Holder.Holder;
      end record;

   package Handler_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Handler_Record);

   package Signal_Maps is
     new WL.String_Maps (Handler_Lists.List, Handler_Lists."=");

   type Signal_Dispatcher is tagged
      record
         Next_Id : Handler_Id := 1;
         Map     : Signal_Maps.Map;
         Object  : Signal_Object_Access;
      end record;

   function Signal_Name (Signal : Signal_Type) return String
   is (Ada.Strings.Unbounded.To_String (Signal.Name));

   function New_Signal (Name : String) return Signal_Type
   is (Signal_Type'
         (Name => Ada.Strings.Unbounded.To_Unbounded_String (Name)));

end Rho.Signals;
