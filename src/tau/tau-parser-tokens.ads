private package Tau.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_Integer_Constant, Tok_Floating_Point_Constant,
       Tok_String_Constant,

       Tok_Abstract, Tok_Begin, Tok_Constant, Tok_End,
       Tok_Function, Tok_Generic, Tok_In, Tok_Is,
       Tok_Material, Tok_Out, Tok_Return,

       Tok_Colon, Tok_Semicolon, Tok_Comma,

       Tok_Equal, Tok_Not_Equal,
       Tok_Greater, Tok_Greater_Equal,
       Tok_Less, Tok_Less_Equal,

       Tok_Asterisk, Tok_Slash, Tok_Plus, Tok_Minus,

       Tok_Left_Paren, Tok_Right_Paren,
       Tok_Dot, Tok_Becomes);

end Tau.Parser.Tokens;
