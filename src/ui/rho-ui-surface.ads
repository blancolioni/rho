with Rho.Rectangles;

package Rho.UI.Surface is

   type Instance is
     abstract new Rho.Rectangles.Rectangle_Interface
   with private;

   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

private

   type Instance is
     abstract new Rho.Rectangles.Rectangle_Interface with
      record
         null;
      end record;

end Rho.UI.Surface;
