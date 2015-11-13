-------------------------------------------------------------------------------
--      File:   syslog-transport-tcp.adb
--
--
--              Copyright (C) Piller
--
--              Florian Fischer, LMS
--------------------------------------------------------------------------------

with Ada.Streams;
with GNAT.Sockets;
--with Ada.Text_IO; use Ada.Text_IO;

package body Syslog.Transport.TCP is
   Socket : GNAT.Sockets.Socket_Type;
   Initialized : Boolean := False;

   procedure Connect
     (Hostname : in String;
      Port : in Natural := 514)
   is
      use GNAT.Sockets;
      Address : Sock_Addr_Type;
      Local_Socket : Socket_Type;
   begin
      Address.Addr := Addresses (Get_Host_By_Name (Hostname), 1);
      Address.Port := Port_Type (Port);
      Create_Socket (Local_Socket);
      Connect_Socket (Local_Socket, Address);

      if Initialized then
         Close_Socket (Socket);
      end if;

      Socket := Local_Socket;
      Initialized := True;
   end Connect;


   procedure Send (Message : in String) is
   begin
      if not Initialized then
         return;
      end if;

      declare
         use type Ada.Streams.Stream_Element_Offset;
         Data : Ada.Streams.Stream_Element_Array (1 .. Message'Length + 1);
         Last : Ada.Streams.Stream_Element_Offset := Data'First - 1;
      begin
         for I in Message'Range loop
            Data (Data'First
                  + Ada.Streams.Stream_Element_Offset (I - Message'First))
              := Character'Pos (Message (I));
         end loop;
         Data(Data'Length) := Character'Pos(ASCII.LF); --delimter

         GNAT.Sockets.Send_Socket (Socket, Data, Last);
         pragma Unreferenced (Last);
      end;
   end Send;
end;
