unit DeepStar.SDL3_Package;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$modeswitch typehelpers}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}
{$ModeSwitch duplicatelocals}

interface

uses
  Classes,
  SysUtils,
  Types,
  libSDL3,
  libSDL3_image,
  libSDL3_ttf,
  libSDL3_mixer,
  System.UITypes,
  DeepStar.Utils,
  DeepStar.DSA.Linear.ArrayList;

{$I DeepStar.SDL3_Package.Utils}
{$I DeepStar.SDL3_Package.Windows}
{$I DeepStar.SDL3_Package.Renderer.inc}
{$I DeepStar.SDL3_Package.Texture}
{$I DeepStar.SDL3_Package.Mixer}
{$I DeepStar.SDL3_Package.Timer}

implementation

{$I DeepStar.SDL3_Package.Utils_impl}
{$I DeepStar.SDL3_Package.Windows_impl}
{$I DeepStar.SDL3_Package.Renderer_impl}
{$I DeepStar.SDL3_Package.Texture_impl} 
{$I DeepStar.SDL3_Package.Mixer_impl}
{$I DeepStar.SDL3_Package.Timer_impl}

end.

