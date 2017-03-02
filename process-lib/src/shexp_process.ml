module Std = struct
  module Process = Process
  [@@deprecated "[since 2017-02] use Shexp_process. \
                 The Std.Process sub-module is no longer needed"]
end
[@@deprecated "[since 2017-02] use Shexp_process. The Std sub-module is no longer needed"]

include Process
