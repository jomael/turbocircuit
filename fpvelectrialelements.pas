unit fpvelectrialelements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //
  fpvectorial;

type
  TWire = class(TvEntity)
  public
    DestPos: T3DPoint;
//    procedure Assign(ASource: TPath);

  end;

implementation

end.

