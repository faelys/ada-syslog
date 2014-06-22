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

package body Syslog.Transport.Send_Task is

   procedure Set_Backend (Send : in Transporter) is
   begin
      Backend := Send;
   end Set_Backend;


   procedure Local_Transport (Message : in String) is
      Send : constant Transporter := Backend;
   begin
      if Send /= null then
         Queue.Append
           (Ada.Strings.Unbounded.To_Unbounded_String (Message),
            Send);
      end if;
   end Local_Transport;



   protected body Queue is
      entry Append
        (Message : in Ada.Strings.Unbounded.Unbounded_String;
         Send : in not null Transporter)
         when True is
      begin
         if Task_Waiting then
            Task_Waiting := False;
            requeue Sender.Send;
         else
            List.Append (Message);
         end if;
      end Append;


      procedure Get
        (Message : out Ada.Strings.Unbounded.Unbounded_String;
         Send : out Transporter) is
      begin
         if List.Is_Empty then
            Send := null;
         else
            Message := List.First_Element;
            Send := Backend;
            List.Delete_First;
         end if;

         Task_Waiting := Send = null;
      end Get;

   end Queue;


   task body Sender is
      Packet : Ada.Strings.Unbounded.Unbounded_String;
      Local_Backend : Transporter;
   begin
      loop
         Queue.Get (Packet, Local_Backend);

         if Local_Backend = null then
            select
               accept Send
                 (Message : in Ada.Strings.Unbounded.Unbounded_String;
                  Transport : in not null Transporter)
               do
                  Packet := Message;
                  Local_Backend := Transport;
               end Send;
            or
               terminate;
            end select;
         end if;

         Local_Backend (Ada.Strings.Unbounded.To_String (Packet));
      end loop;
   end Sender;

end Syslog.Transport.Send_Task;
