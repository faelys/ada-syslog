------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Streams;
with GNAT.Sockets;

package body Syslog.Transport.UDP is

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
      Create_Socket (Local_Socket, Address.Family, Socket_Datagram);
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
         Data : Ada.Streams.Stream_Element_Array (1 .. Message'Length);
         Last : Ada.Streams.Stream_Element_Offset := Data'First - 1;
      begin
         for I in Message'Range loop
            Data (Data'First
              + Ada.Streams.Stream_Element_Offset (I - Message'First))
              := Character'Pos (Message (I));
         end loop;

         GNAT.Sockets.Send_Socket (Socket, Data, Last);
         pragma Unreferenced (Last);
      end;
   end Send;

end Syslog.Transport.UDP;
