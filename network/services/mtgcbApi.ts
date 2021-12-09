import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';
import { SetCategory, SetType } from '../../features/browse/browseSlice';
import { Card } from '../../features/browse/CardBox';
import buildBrowseFilter from '../features/browse/buildBrowseFilter';
import buildBrowseExpansionFilter from '../features/browse/buildExpansionBrowseFilter';
import determineDistinctClause from '../features/browse/determineDistinctClause';
import { determineSortFilter } from '../features/browse/filters';
import { allCards, allCardsMeta, allSetNames, allSets, allSetsMeta, setTypes as setTypesQuery } from '../queries/index';
import { SearchOptions } from '../types';
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
  slug: string;
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

interface SetBySlugVariables {
  slug: string;
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

interface AllCardsResponse {
  allCards: Card[];
}

interface AllCardsMetaResponse {
  _allCardsMeta: {
    count: number;
  };
}

export const mtgcbApi = createApi({
  reducerPath: 'mtgcb',
  baseQuery: fetchBaseQuery({ baseUrl: process.env.NEXT_PUBLIC_MTGCB_API_URL }),
  tagTypes: ['Sets', 'Cards'],
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
    getSetBySlug: builder.query<AxiosResponse<SetResponse>, SetBySlugVariables>({
      query: ({ slug }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSets,
          variables: {
            where: { slug },
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
    getAllCards: builder.query<AxiosResponse<AllCardsResponse>, SearchOptions>({
      query: ({
        first,
        skip,
        sortBy,
        name,
        oracleTextQuery,
        cardSets,
        cardRarities,
        cardTypes,
        cardColors,
        showAllPrintings,
        cardStatSearches,
        sortByDirection,
      }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allCards,
          variables: {
            first,
            skip,
            sortBy: determineSortFilter(sortBy, sortByDirection),
            name,
            where: buildBrowseFilter({ cardSets, cardRarities, cardTypes, cardColors, oracleTextQuery, cardStatSearches }),
            distinct: determineDistinctClause(showAllPrintings, sortBy),
          },
        },
      }),
    }),
    getAllCardsMeta: builder.query<AxiosResponse<AllCardsMetaResponse>, SearchOptions>({
      query: ({
        first,
        skip,
        sortBy,
        name,
        oracleTextQuery,
        cardSets,
        cardRarities,
        cardTypes,
        cardColors,
        showAllPrintings,
        cardStatSearches,
        sortByDirection,
      }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allCardsMeta,
          variables: {
            first,
            skip,
            sortBy: determineSortFilter(sortBy, sortByDirection),
            name,
            where: buildBrowseFilter({ cardSets, cardRarities, cardTypes, cardColors, oracleTextQuery, cardStatSearches }),
            distinct: determineDistinctClause(showAllPrintings, sortBy),
          },
        },
      }),
    }),
  }),
});

export const {
  useGetAllSetsQuery,
  useGetAllSetsMetaQuery,
  useGetSetBySlugQuery,
  useGetAllSetNamesQuery,
  useGetSetTypesQuery,
  useGetAllCardsQuery,
  useGetAllCardsMetaQuery,
} = mtgcbApi;
