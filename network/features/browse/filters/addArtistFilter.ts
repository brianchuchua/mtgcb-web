interface AddArtistFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (artistQuery: string, where: { AND: any[] }): any;
}

const addArtistFilter: AddArtistFilterFunction = (artistQuery, where) => {
  if (artistQuery) {
    const graphqlFilters = { AND: [] };

    graphqlFilters.AND.push({ artistIds: { some: { name: { contains: artistQuery, mode: 'insensitive' } } } });

    where.AND.push(graphqlFilters);
  }
};

export default addArtistFilter;
