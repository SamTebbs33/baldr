# baldr

A small experimental version control system.

**Note:** This is purely experimental and is not intended for serious use in its current state, you have been warned!

## Download

Download from the [releases page](https://github.com/SamTebbs33/baldr/releases).

## Usage

Replace below usages of the `baldr` command with `java -jar baldr.jar`

### init
Initialises an empty baldr repository in the current directory.

```
baldr init
```

### stage
Add a file to the staging list.

```
baldr stage path/to/file
```

## unstage
Remove a file from the staging list

### save
Saves the state of all staged files

```
baldr save "message"
```

### revert
Revert the working directory to the specified save

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
