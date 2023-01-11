with Rho.Objects;

package Rho.Rectangles is

   type Rectangle_Interface is limited interface;

   function X
     (Rectangle : Rectangle_Interface)
      return Real
      is abstract;

   function Y
     (Rectangle : Rectangle_Interface)
      return Real
      is abstract;

   function Width
     (Rectangle : Rectangle_Interface)
      return Non_Negative_Real
      is abstract;

   function Height
     (Rectangle : Rectangle_Interface)
      return Non_Negative_Real
      is abstract;

   function Aspect_Ratio
     (Rectangle : Rectangle_Interface'Class)
      return Non_Negative_Real;

   type Rectangle_Object is
     abstract new Rho.Objects.Root_Object_Type
     and Rectangle_Interface
   with private;

   overriding function X (Rectangle : Rectangle_Object) return Real;
   overriding function Y (Rectangle : Rectangle_Object) return Real;
   overriding function Width
     (Rectangle : Rectangle_Object)
      return Non_Negative_Real;

   overriding function Height
     (Rectangle : Rectangle_Object)
      return Non_Negative_Real;

   procedure Initialize_Rectangle
     (Rectangle     : in out Rectangle_Object'Class;
      X, Y          : Real;
      Width, Height : Non_Negative_Real);

private

   function Aspect_Ratio
     (Rectangle : Rectangle_Interface'Class)
      return Non_Negative_Real
   is (Rectangle.Width / Rectangle.Height);

   type Rectangle_Object is
     abstract new Rho.Objects.Root_Object_Type
     and Rectangle_Interface with
      record
         X, Y : Real := 0.0;
         Width, Height : Non_Negative_Real := 1.0;
      end record;

   overriding function X (Rectangle : Rectangle_Object) return Real
   is (Rectangle.X);

   overriding function Y (Rectangle : Rectangle_Object) return Real
   is (Rectangle.Y);

   overriding function Width
     (Rectangle : Rectangle_Object)
      return Non_Negative_Real
   is (Rectangle.Width);

   overriding function Height
     (Rectangle : Rectangle_Object)
      return Non_Negative_Real
   is (Rectangle.Height);

end Rho.Rectangles;
