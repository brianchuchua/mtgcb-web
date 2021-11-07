import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';
import { SetCategory, SetType } from '../../features/browse/browseSlice';
import buildBrowseExpansionFilter from '../features/browse/buildExpansionBrowseFilter';
import { determineSortFilter } from '../features/browse/filters';
import { allSetNames, allSets, allSetsMeta, setTypes as setTypesQuery } from '../queries/index';
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
  sealedProductUrl: string;
  isDraftable: boolean;
}

interface SetResponse {
  allSets: Set[];
}

interface AllSetsVariables {
  first: number;
  skip: number;
  sortBy?: string;
  name: string;
  sortByDirection: 'ASC' | 'DESC';
  setTypes: SetType[];
  setCategories: SetCategory[];
}

interface SetsMetaResponse {
  _allSetsMeta: {
    count: number;
  };
}

interface AllSetsMetaVariables {
  sortBy?: string;
  name: string;
  sortByDirection: 'ASC' | 'DESC';
  setTypes: SetType[];
  setCategories: SetCategory[];
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

interface SetTypesResponse {
  setTypes: SetType[];
}

export const mtgcbApi = createApi({
  reducerPath: 'mtgcb',
  baseQuery: fetchBaseQuery({ baseUrl: process.env.NEXT_PUBLIC_MTGCB_API_URL }),
  tagTypes: ['Sets'],
  endpoints: (builder) => ({
    getAllSets: builder.query<AxiosResponse<SetResponse>, AllSetsVariables>({
      query: ({ first, skip, name, sortBy = 'releasedAt', sortByDirection = 'DESC', setTypes, setCategories }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSets,
          variables: {
            first,
            skip,
            sortBy: determineSortFilter(sortBy, sortByDirection),
            name,
            where: buildBrowseExpansionFilter({ setTypes, setCategories }),
          },
        },
      }),
    }),
    getAllSetsMeta: builder.query<AxiosResponse<SetsMetaResponse>, AllSetsMetaVariables>({
      query: ({ name, sortBy = 'releasedAt', sortByDirection = 'DESC', setTypes, setCategories }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSetsMeta,
          variables: {
            sortBy: determineSortFilter(sortBy, sortByDirection),
            name,
            where: buildBrowseExpansionFilter({ setTypes, setCategories }),
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
    getSetTypes: builder.query<AxiosResponse<SetTypesResponse>, void>({
      query: () => ({
        url: '',
        method: 'POST',
        body: {
          query: setTypesQuery,
        },
      }),
    }),
  }),
});

export const { useGetAllSetsQuery, useGetAllSetsMetaQuery, useGetAllSetNamesQuery, useGetSetTypesQuery } = mtgcbApi;
