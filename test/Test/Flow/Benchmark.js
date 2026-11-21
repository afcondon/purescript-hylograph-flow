export const milliseconds = () => performance.now();
// Run by `node -e`, as the head of the module says: there is no script
// path, so the words begin at the second place.
export const commandLine = () => process.argv.slice(1);
