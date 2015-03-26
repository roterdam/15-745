{-
  Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
  Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
               Connor Brem <cbrem@andrew.cmu.edu>

  This file contains functions for pasing command-line arguments.
-}
module Args (parseArgs, usage) where

import Job
import Util
import System.Console.GetOpt
import System.FilePath
import qualified Data.Maybe as Maybe

import Debug.Trace (trace)

-- Usage info for the the program with the given name.
usage :: String -> String
usage prog =
  let title = "Usage for '" ++ prog ++ "':"
  in  usageInfo title argTable

-- Builds a Job from command-line arugments.
parseArgs :: [String] -> Either String Job
parseArgs args =
  case getOpt Permute argTable args of
    (_,     _,    err : _ ) -> Left err
    (opts, [src], _       ) -> Right $ inferFields $ (foldr ($) defaultJob opts) {src = src}
    (_,     _,    _       ) -> Left "Wrong number of arguments"
  where -- If the user speficies no output file, infer an output file name.
        -- If the user specifies an output file, infer an output format (even if a format was given).
        inferFields :: Job -> Job
        inferFields job =
          if null $ dest job
            then  let ext = Maybe.fromJust $ lookup (fmt job) fmtToExt
                  in job {dest = replaceExtension (src job) ext}
            else case lookup (takeExtension $ dest job) extToFmt of
              Nothing -> job
              Just fmt -> job {fmt = fmt}

        fmtToExt :: [(OF, String)]
        fmtToExt = [(C0,       ".c0"),
                    (AST,      ".ast"),
                    (PCT,      ".pct"),
                    (OptPCT,   ".opct"),
                    (IRT,      ".irt"),
                    (OptIRT,   ".oirt"),
                    (B3Asm,    ".thr"),
                    (SSA3Asm,  ".ssa"),
                    (OptSsa,   ".ossa"),
                    (B2Asm,    ".two"),
                    (XAsm,     ".s")]

        extToFmt :: [(String, OF)]
        extToFmt = map swap fmtToExt


-- Command-line arguments.
argTable :: [OptDescr (Job -> Job)]
argTable = [
  Option ['o']  ["out"]           (ReqArg setDest "<dest_path>")  "Redirects output of the compiler to a particular target file. Will attempt to autodetect output type.",
  Option ['l']  ["library"]       (ReqArg setLib "<lib_path>")    "Specifies a library to load along with the source file.",
  Option ['O']  ["optimization"]  (ReqArg setLvl "<n>")           "Sets the optimization level to <n>. Valid values of <n> are 0 (no optimization), 1 (some optimizations), and 2 (all optimizations).",
  Option ['v']  ["verbose"]       (NoArg setVerb)                 "Turns on verbose errors. Errors are not verbose by default.",
  Option []     ["safe"]          (NoArg $ setSafe True)          "Specifies that the compiler should check for expections at runtime.",
  Option []     ["unsafe"]        (NoArg $ setSafe False)         "Speicifes that the compiler should NOT check for exceptions at runtime.",
  Option ['c']  ["c0"]            (NoArg $ setFmt C0)             "Sets the output type to be C0 (act as a pretty printer).",
  Option ['a']  ["ast"]           (NoArg $ setFmt AST)            "Produces an abstract syntax tree.",
  Option ['p']  ["pct"]           (NoArg $ setFmt PCT)            "Produces a PC tree.",
  Option ['P']  ["optPCT"]        (NoArg $ setFmt OptPCT)         "Produces a optimized PC tree.",
  Option ['i']  ["irt"]           (NoArg $ setFmt IRT)            "Produces an IR tree.",
  Option ['I']  ["optIRT"]        (NoArg $ setFmt OptIRT)         "Produces an optimized IR tree.",
  Option ['3']  ["b3asm"]         (NoArg $ setFmt B3Asm)          "Produces abstract 3-argument assembly.",
  Option ['z']  ["ssa3asm"]       (NoArg $ setFmt SSA3Asm)        "Produces abstract 3-argument assembly in SSA form.",
  Option ['Z']  ["optSsa"]        (NoArg $ setFmt OptSsa)         "Produces optimized 3-argument SSA assembly.",
  Option ['2']  ["b2asm"]         (NoArg $ setFmt B2Asm)          "Produces abstract 2-argument assembly.",
  Option ['s']  ["xasm"]          (NoArg $ setFmt XAsm)           "Produces x86-64 assembly."]
  where setFmt :: OF -> Job -> Job
        setFmt fmt job = job {fmt = fmt}

        setVerb :: Job -> Job
        setVerb job = job {verb = True}

        setSafe :: Bool -> Job -> Job
        setSafe safe job = job {safe = safe}

        setLib :: String -> Job -> Job
        setLib lib job = job {lib = Just lib}

        setDest :: String -> Job -> Job
        setDest dest job = job {dest = dest}

        setLvl :: String -> Job -> Job
        setLvl lvl job = job {lvl = read lvl}
