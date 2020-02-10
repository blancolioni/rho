with Interfaces.C;

package GL_Types is

   type Ubyte is new Interfaces.C.unsigned_char;
   type Int is new Interfaces.C.int;
   type Uint is new Interfaces.C.unsigned;
   type Sizei is new Interfaces.C.int;
   type Sizeiptr is new Interfaces.C.ptrdiff_t;

   type Bit_Mask is new Uint;

   type GLboolean is new Interfaces.C.unsigned_char;
   type GLfloat is new Interfaces.C.C_float;
   type GLdouble is new Interfaces.C.double;

   subtype Clampf is GLfloat;
   subtype Clampd is GLfloat;

   type GLenum is new Uint;

   type Array_Of_Int is array (Positive range <>) of aliased Int;
   pragma Convention (C, Array_Of_Int);

   type Array_Of_Float is array (Positive range <>) of aliased GLfloat;
   pragma Convention (C, Array_Of_Float);

   type Array_Of_Double is array (Positive range <>) of aliased GLdouble;
   pragma Convention (C, Array_Of_Double);

   subtype Float_Matrix_4x4 is Array_Of_Float (1 .. 16);

end GL_Types;
