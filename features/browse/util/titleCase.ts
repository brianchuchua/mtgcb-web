const titleCase = (str: string): string =>
  str
    ? str
        .toLowerCase()
        .split('_')
        .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
        .join(' ')
    : 'N/A';

export default titleCase;
