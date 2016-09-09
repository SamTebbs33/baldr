# baldr

A small experimental version control system.

## Usage
### init
Initialises an empty baldr repository in the current directory.

### stage
Add a file to the staging list.

```
baldr stage path/to/file
```

### save
Saves the state of all staged files

```
baldr save "message"
```

### revert
Revert a save's files to how they were in the save

```
baldr revert "save_hash"
```

### list
List the hashes, dates and messages of all saves

```
baldr list
```

### ignore
Add a file to the ignore list. All files in the ignore list cannot be staged.

```
baldr ignore path/to/file
```

### acknowledge
Remove a file from the ignore list.

```
baldr ack path/to/file
```