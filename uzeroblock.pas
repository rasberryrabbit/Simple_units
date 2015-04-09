unit uzeroblock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ScanZeroBlock(buf : Pchar; startpos, len:Integer; var FindPos:Integer):Integer;

implementation

function ScanZeroBlock(buf : Pchar; startpos, len:Integer; var FindPos:Integer):Integer;
var
  i : Integer;
  d4 : longword;
  d2 : word;
begin
  Result:=0;
  FindPos:=-1;
  Inc(buf,startpos);
  Dec(len,startpos);
  while len>0 do begin
    if len>3 then begin
      d4 := PLongword(buf)^;
      {$ifndef ENDIAN_LITTLE}
      d4:=Swap(d4);
      {$endif}
      if d4=0 then begin
        if FindPos=-1 then
           FindPos:=startpos;
        Inc(Result,4);
        Inc(StartPos,4);
      end else begin
        for i:=0 to 3 do begin
          if d4 and $ff=0 then begin
            if FindPos=-1 then
               FindPos:=startpos;
            Inc(Result);
          end else begin
            if FindPos>-1 then
               exit;
          end;
          d4:=d4 shr 8;
          Inc(startpos);
        end;
      end;
      Dec(len,4);
      Inc(buf,4);
    end else if len>1 then begin
      d2 := PWord(buf)^;
      {$ifndef ENDIAN_LITTLE}
      d2:=Swap(d2);
      {$endif}
      if d2=0 then begin
        if FindPos=-1 then
           FindPos:=startpos;
        Inc(Result,2);
        Inc(StartPos,2);
      end else begin
        for i:=0 to 1 do begin
          if d2 and $ff=0 then begin
            if FindPos=-1 then
               FindPos:=startpos;
            Inc(Result);
          end else begin
            if FindPos>-1 then
               exit;
          end;
          d2:=d2 shr 8;
          Inc(startpos);
        end;
      end;
      Dec(len,2);
      Inc(buf,2);
    end else if len>0 then begin
      if byte(buf^) and $ff=0 then begin
        if FindPos=-1 then
           FindPos:=startpos;
        Inc(Result);
      end else begin
        if FindPos>-1 then
           exit;
      end;
      Dec(len);
    end;
  end;

end;

end.

