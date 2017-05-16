unit tornament_dsgn1_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Shape1ChangeBounds(Sender: TObject);
    procedure OnLabelClick(Sender:TObject);
    procedure OnLabelClickArray(Sender:TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses ulabelbox;

{$R *.lfm}

var
  lbarray : TLabelBoxArray = nil;

{ TForm1 }

procedure TForm1.Shape1ChangeBounds(Sender: TObject);
begin

end;

procedure TForm1.OnLabelClick(Sender: TObject);
begin
  //ShowMessage(TLabelBox(Sender).Caption);
end;

procedure TForm1.OnLabelClickArray(Sender: TObject);
begin
  Label2.Caption:=(Sender as TLabelBox).Caption;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TLabelBox.Create(self) do begin
    Parent:=self;
    Top:=Random(100)+10;
    Left:=Random(100)+10;
    Caption:='test'+Format('%d',[Random(1000)]);
    OnClick:=@OnLabelClick;
    AutoFit:=False;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if lbarray<>nil then begin
    lbarray.ReArrangeLabels;
    lbarray.Marked[4]:=lbarray.Marked[3];
    lbarray.DeleteLabel(True);
    lbarray.InsertLabel(1,Format('test %d',[Random(15000)]));
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if lbarray<>nil then begin
    lbarray.Clear;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  with TLabelBoxLine.Create(self) do begin
    Parent:=self;
    Top:=Random(100)+10;
    Left:=Random(100)+10;
    Caption:='test'+Format('%d',[Random(1000)]);
    OnClick:=@OnLabelClick;
    LineWidth:=3;
    LineDrection:=0;
    LineLength:=100;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormShow(Sender: TObject);
var
  i:Integer;
begin
  lbarray:=TLabelBoxArray.Create(self);
  with lbarray do begin
    Parent:=self;
    Top:=200;
    Left:=50;
    LabelMarginX:=10;
    for i:=0 to 10 do
      AddLabel('test'+Format('%d',[i]));
    OnLabelClick:=@OnLabelClickArray;
  end;
end;

end.

