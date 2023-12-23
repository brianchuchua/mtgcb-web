interface AddSubsetFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (subsets: string[], where: { AND: any[] }): any;
}

const addSubsetFilter: AddSubsetFilterFunction = (subsets, where) => {
  if (subsets?.length && subsets[0] !== 'All') {
    const setIdConditions = { setId: { OR: [] } };
    for (const subset of subsets) {
      setIdConditions.setId.OR.push({ id: { equals: subset } });
    }
    where.AND[0] = setIdConditions; // overrides main set filter
  }
};

export default addSubsetFilter;
