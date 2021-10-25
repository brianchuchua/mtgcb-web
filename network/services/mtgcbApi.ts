import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';
import { allSetNames, allSets, allSetsMeta } from '../queries/index';

// TODO: Code split these types for readability

interface AxiosResponse<T> {
  data: T;
}

interface Set {
  id: string;
  name: string;
  code: string;
  category: string;
  setType: string;
  cardCount: number;
  releasedAt: string;
}

interface SetResponse {
  allSets: Set[];
}

interface AllSetsVariables {
  first: number;
  skip: number;
  sortBy?: string;
}

interface SetsMetaResponse {
  _allSetsMeta: {
    count: number;
  };
}

interface AllSetsMetaVariables {
  sortBy?: string;
}

interface SetName {
  id: string;
  name: string;
}

interface SetNamesResponse {
  allSets: SetName[];
}

interface AllSetNamesVariables {
  sortBy?: string;
}

export const mtgcbApi = createApi({
  reducerPath: 'mtgcb',
  baseQuery: fetchBaseQuery({ baseUrl: process.env.NEXT_PUBLIC_MTGCB_API_URL }),
  tagTypes: ['Sets'],
  endpoints: (builder) => ({
    getAllSets: builder.query<AxiosResponse<SetResponse>, AllSetsVariables>({
      query: ({ first, skip, sortBy = 'releasedAt_DESC' }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSets,
          variables: {
            first,
            skip,
            sortBy,
          },
        },
      }),
    }),
    getAllSetsMeta: builder.query<AxiosResponse<SetsMetaResponse>, AllSetsMetaVariables>({
      query: ({ sortBy = 'releasedAt_DESC' }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSetsMeta,
          variables: {
            sortBy,
          },
        },
      }),
    }),
    getAllSetNames: builder.query<AxiosResponse<SetNamesResponse>, AllSetNamesVariables>({
      query: ({ sortBy = 'releasedAt_DESC' }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSetNames,
          variables: {
            sortBy,
          },
        },
      }),
    }),
  }),
});

export const { useGetAllSetsQuery, useGetAllSetsMetaQuery, useGetAllSetNamesQuery } = mtgcbApi;
