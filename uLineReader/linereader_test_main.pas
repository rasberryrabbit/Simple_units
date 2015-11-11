unit linereader_test_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uLineReader;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  fstm : TFileStream;
  liner : TLineReader;
  stemp : string;
begin
  if OpenDialog1.Execute then begin
    fstm := TFileStream.Create(OpenDialog1.FileName,fmOpenRead);
    try
      liner := TLineReader.Create;
      try
        liner.Stream:=fstm;
        while not liner.Eof do begin
          liner.ReadLine(stemp);
          stemp:=liner.ToUTF8(stemp);
          Memo1.Lines.Add(stemp);
        end;
      finally
        liner.Free;
      end;
    finally
      fstm.Free;
    end;
  end;
end;

end.

