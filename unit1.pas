unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Menus,ShellApi;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboPromt: TComboBox;
    ComboModel: TComboBox;
    GroupBox1: TGroupBox;
    GroupBoxModel: TGroupBox;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MemoAnswer: TMemo;
    MemoAsk: TMemo;
    MenuFile: TMenuItem;
    MenuClose: TMenuItem;
    MenuItem1: TMenuItem;
    MenuClearAll: TMenuItem;
    PanelRight: TPanel;
    Panel2: TPanel;
    PanelBottom: TPanel;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuClearAllClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private

  public
  end;

var
  Form1: TForm1;

implementation
uses
  OllamaAPI, unit2;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://ollama.com/', nil, nil, 1);
end;

procedure TForm1.Label1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Label1.color := clPurple
end;

procedure TForm1.MenuClearAllClick(Sender: TObject);
begin
   MemoAnswer.Clear;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  Form2.ShowModal;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if ComboModel.Text = '' then
  begin
    ShowMessage('Wybierz model z listy!');
    Exit;
  end;
  GenerateResponse;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  ChatWithModel;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
   if ComboModel.Text = '' then
  begin
    ShowMessage('Wybierz model z listy!');
    Exit;
  end;
  GenerateResponse;
end;



end.

