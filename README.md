Sweet Tooth
===========

Sweet tooth locates potential application sites for syntactic abstraction.
Run sweet tooth by calling the shell script `sweet-tooth.sh`. The script expects three arguments

1. The source path to some SugarJ files.
2. The path of a SugarJ source file relative to the source path.
3. The name of a transformation rule that is part of the SugarJ source file.

Sweet tooth will then derive a pattern for code that can be generated
by the given transformation rule.
