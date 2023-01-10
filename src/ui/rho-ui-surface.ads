private with Cairo;

private with Rho.Tiled_Regions;
private with Rho.Nodes;
private with Rho.Meshes;
private with Rho.Scenes;

with Rho.Bitmaps;
with Rho.Color;
with Rho.Rectangles;
with Rho.Render;

package Rho.UI.Surface is

   type Instance is
     new Rho.Rectangles.Rectangle_Interface with private;

   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Create
     (X, Y          : Real;
      Width, Height : Non_Negative_Real)
      return Reference;

   procedure Destroy
     (This : in out Reference);

   procedure Render
     (This : not null access Instance'Class;
      Target : not null access Rho.Render.Render_Target'Class);

   type Context is tagged private;

   function Create
     (This : not null access Instance'Class;
      X, Y : Real;
      W, H : Non_Negative_Real)
      return Context;

   function Create
     (From : Context'Class;
      X, Y : Real;
      W, H : Non_Negative_Real)
      return Context;

   type Draw_Primitive is abstract tagged private;

   procedure Bounding_Box
     (This          : Draw_Primitive;
      DX, DY        : out Real;
      Width, Height : out Non_Negative_Real)
   is abstract;

   procedure Draw
     (This    : Draw_Primitive;
      Target  : in out Context'Class)
   is abstract;

   procedure Apply
     (This : in out Context;
      Primitive : Draw_Primitive'Class);

   procedure Draw_Color
     (This  : in out Context;
      Color : Rho.Color.Color_Type);

   procedure Move_To
     (This : in out Context;
      X, Y : Real);

   procedure Line_To
     (This : in out Context;
      X, Y : Real);

   procedure Line
     (This   : in out Context;
      X1, Y1 : Real;
      X2, Y2 : Real);

   procedure Rectangle
     (This          : in out Context;
      Width, Height : Non_Negative_Real;
      Stencil       : Rho.Bitmaps.Greyscale_Reference);

private

   package Mesh_Tiles is
     new Rho.Tiled_Regions (Rho.Meshes.Mesh_Type);

   type Instance is
     new Rho.Rectangles.Root_Rectangle_Type with
      record
         Scene   : Rho.Scenes.Scene_Type;
         Top     : Rho.Nodes.Node_Type;
         Tiles   : Mesh_Tiles.Tiled_Region;
         Surface : Cairo.Cairo_Surface;
      end record;

   type Draw_Primitive is abstract tagged null record;

   type Context is tagged
      record
         Surface       : Reference;
         Left, Top     : Real;
         Right, Bottom : Real;
         X, Y          : Real;
         Color         : Rho.Color.Color_Type := Rho.Color.Black;
      end record;

end Rho.UI.Surface;
