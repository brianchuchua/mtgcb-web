import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';
import { CardColors, CardRarity, CardSet, CardStatSearch, CardType, SetCategory, SetType } from '../../features/browse/browseSlice';
import { SetSummary } from '../../features/browse/SetBox';
import { Card } from '../../features/browse/types/Card';
import { SetCompletionStatus } from '../../features/collections/collectionSlice';
import buildBrowseFilter, { buildAdditionalWhereFilter } from '../features/browse/buildBrowseFilter';
import buildBrowseExpansionFilter from '../features/browse/buildExpansionBrowseFilter';
import { CardAutocompleteOptions, SearchOptions } from '../features/browse/commonTypes';
import determineDistinctClause from '../features/browse/determineDistinctClause';
import { determineAdditionalSortFilter, determineSortFilter } from '../features/browse/filters';
import {
  allCards,
  allCardsMeta,
  allSetNames,
  allSets,
  allSetsMeta,
  allSubsets,
  allSubsetsByGroupId,
  cardAutocomplete,
  cardsFromSubsets,
  collectionByCardIdLegacy,
  collectionSummaryLegacy,
  costToPurchaseAll,
  filteredCardsSummaryLegacy,
  filteredCollectionSummaryLegacy,
  setSummaryLegacy,
  setTypes as setTypesQuery,
  tcgplayerMassImportForUserLegacy,
  updateCollectionLegacy,
} from '../queries/index';

// TODO: Split these up for readability

export const mtgcbApi = createApi({
  reducerPath: 'mtgcb',
  baseQuery: fetchBaseQuery({ baseUrl: process.env.NEXT_PUBLIC_MTGCB_API_URL, credentials: 'include' }),
  tagTypes: ['Sets', 'Cards', 'Collections', 'CollectionSummary'],
  endpoints: (builder) => ({
    getAllSets: builder.query<AxiosResponse<SetResponse>, AllSetsVariables>({
      query: ({
        first,
        skip,
        name,
        sortBy = 'releasedAt',
        sortByDirection = 'desc',
        includeSubsets = false,
        includeSubsetGroups = true,
        setTypes,
        setCategories,
      }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSets,
          variables: {
            take: first,
            skip,
            orderBy: determineSortFilter(sortBy, sortByDirection),
            where: buildBrowseExpansionFilter({
              name,
              setTypes,
              setCategories,
              includeSubsets: convertToBoolean(includeSubsets),
              includeSubsetGroups: convertToBoolean(includeSubsetGroups),
            }),
          },
        },
      }),
    }),
    // TODO: What do I do about meta calls? Can I just get this data from the base call now? Lots of touch points in the code.
    getAllSetsMeta: builder.query<AxiosResponse<SetsMetaResponse>, AllSetsMetaVariables>({
      query: ({
        name,
        sortBy = 'releasedAt',
        sortByDirection = 'desc',
        includeSubsets = false,
        includeSubsetGroups = true,
        setTypes,
        setCategories,
      }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSetsMeta,
          variables: {
            where: buildBrowseExpansionFilter({
              name,
              setTypes,
              setCategories,
              includeSubsets: convertToBoolean(includeSubsets),
              includeSubsetGroups: convertToBoolean(includeSubsetGroups),
            }),
          },
        },
      }),
    }),
    getFilteredCollectionSummaryLegacy: builder.query<
      AxiosResponse<FilteredCollectionSummaryLegacyResponse>,
      FilteredCollectionSummaryLegacyVariables
    >({
      providesTags: ['CollectionSummary'],
      query: ({
        userId,
        priceType = 'market',
        first = 50,
        skip = 0,
        search = '',
        sortBy = 'releasedAt',
        sortByDirection = 'desc',
        includeSubsets = false,
        includeSubsetGroups = true,
        additionalSortBy,
        whereSetCompletionStatus,
        setTypes,
        setCategories,
        includeSubsetsInSets = false,
      }) => ({
        url: '',
        method: 'POST',
        body: {
          query: filteredCollectionSummaryLegacy,
          variables: {
            userId: Number(userId),
            priceType,
            take: first,
            skip,
            search,
            orderBy: determineSortFilter(sortBy, sortByDirection),
            additionalSortBy: determineAdditionalSortFilter(sortBy, sortByDirection),
            whereSetCompletionStatus,
            where: buildBrowseExpansionFilter({
              setTypes,
              setCategories,
              includeSubsets: convertToBoolean(includeSubsets),
              includeSubsetGroups: convertToBoolean(includeSubsetGroups),
            }),
            includeSubsetsInSets: convertToBoolean(includeSubsetsInSets),
          },
        },
      }),
    }),
    getFilteredCardsSummaryLegacy: builder.query<AxiosResponse<FilteredCardsSummaryLegacyResponse>, FilteredCardsSummaryLegacyVariables>({
      providesTags: ['Collections'],
      keepUnusedDataFor: 0,
      query: ({
        userId,
        setId,
        first,
        skip,
        sortBy,
        sortByDirection,
        name,
        oracleTextQuery,
        artistQuery,
        cardSets,
        cardRarities,
        cardTypes,
        cardColors,
        showAllPrintings,
        cardStatSearches,
        additionalSortBy,
        additionalWhere,
        subsets,
      }) => ({
        url: '',
        method: 'POST',
        body: {
          query: filteredCardsSummaryLegacy,
          variables: {
            userId: Number(userId),
            setId: Number(setId) || null,
            where: buildBrowseFilter({
              cardSets,
              cardRarities,
              cardTypes,
              cardColors,
              oracleTextQuery: oracleTextQuery ? oracleTextQuery.trim() : '',
              artistQuery: artistQuery ? artistQuery.trim() : '',
              cardStatSearches,
              orderBy: sortBy,
              subsets,
            }),
            search: name ? name.trim() : '',
            orderBy: determineSortFilter(sortBy, sortByDirection),
            take: first,
            skip,
            additionalSortBy: determineAdditionalSortFilter(sortBy, sortByDirection),
            additionalWhere: buildAdditionalWhereFilter({ cardStatSearches }),
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
            where: { slug: { equals: slug } },
          },
        },
      }),
    }),
    getAllSetNames: builder.query<AxiosResponse<SetNamesResponse>, AllSetNamesVariables>({
      query: ({ sortBy = [{ releasedAt: 'desc' }] }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSetNames,
          variables: {
            orderBy: sortBy,
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
        artistQuery,
        cardSets,
        cardRarities,
        cardTypes,
        cardColors,
        showAllPrintings,
        cardStatSearches,
        sortByDirection,
        subsets,
      }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allCards,
          variables: {
            take: first,
            skip,
            orderBy: determineSortFilter(sortBy, sortByDirection),
            where: buildBrowseFilter({
              name: name?.trim(),
              cardSets,
              cardRarities,
              cardTypes,
              cardColors,
              oracleTextQuery: oracleTextQuery ? oracleTextQuery.trim() : '',
              artistQuery: artistQuery ? artistQuery.trim() : '',
              cardStatSearches,
              orderBy: sortBy,
              subsets,
            }),
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
        artistQuery,
        cardSets,
        cardRarities,
        cardTypes,
        cardColors,
        showAllPrintings,
        cardStatSearches,
        sortByDirection,
        subsets,
      }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allCardsMeta,
          variables: {
            where: buildBrowseFilter({
              name: name?.trim(),
              cardSets,
              cardRarities,
              cardTypes,
              cardColors,
              oracleTextQuery: oracleTextQuery ? oracleTextQuery.trim() : '',
              artistQuery: artistQuery ? artistQuery.trim() : '',
              cardStatSearches,
              orderBy: sortBy,
              subsets,
            }),
          },
        },
      }),
    }),
    getCostToPurchaseAll: builder.query<AxiosResponse<CostToPurchaseAllResponse>, void>({
      query: () => ({
        url: '',
        method: 'POST',
        body: {
          query: costToPurchaseAll,
        },
      }),
    }),
    getCollectionSummaryLegacy: builder.query<AxiosResponse<CollectionSummaryLegacyResponse>, CollectionSummaryLegacyVariables>({
      query: ({ userId }) => ({
        url: '',
        method: 'POST',
        body: {
          query: collectionSummaryLegacy,
          variables: {
            userId: Number(userId),
          },
        },
      }),
      providesTags: ['CollectionSummary'],
    }),
    getTcgplayerMassImportForUserLegacy: builder.query<
      // TODO: Remove me, not a great fit for redux query
      AxiosResponse<TcgplayerMassImportForUserLegacyResponse>,
      TcgplayerMassImportForUserLegacyVariables
    >({
      query: ({ setId, userId, includeSubsetsInSets = false }) => ({
        url: '',
        method: 'POST',
        body: {
          query: tcgplayerMassImportForUserLegacy,
          variables: {
            setId: Number(setId),
            userId: Number(userId),
            includeSubsetsInSets: convertToBoolean(includeSubsetsInSets),
          },
        },
      }),
    }),
    getSetSummaryLegacy: builder.query<AxiosResponse<SetSummaryLegacyResponse>, SetSummaryLegacyVariables>({
      query: ({ setId, userId }) => ({
        url: '',
        method: 'POST',
        body: {
          query: setSummaryLegacy,
          variables: {
            setId: Number(setId),
            userId: Number(userId),
          },
        },
      }),
      providesTags: ['CollectionSummary'],
    }),
    getCollectionByCardIdLegacy: builder.query<AxiosResponse<CollectionByCardIdLegacyResponse>, CollectionByCardIdLegacyVariables>({
      query: ({ cardIds, userId }) => ({
        url: '',
        method: 'POST',
        body: {
          query: collectionByCardIdLegacy,
          variables: {
            cardIds: cardIds.map((cardId) => Number(cardId)),
            userId: Number(userId),
          },
        },
      }),
      providesTags: ['Collections'],
      keepUnusedDataFor: 0,
    }),
    updateCollectionLegacy: builder.mutation<AxiosResponse<UpdateCollectionLegacyResponse>, UpdateCollectionLegacyVariables>({
      query: ({ cardId, mode, quantityRegular, quantityFoil, setId, userId }) => ({
        url: '',
        method: 'POST',
        body: {
          query: updateCollectionLegacy,
          variables: {
            cardId: Number(cardId),
            mode,
            quantityRegular: Number(quantityRegular),
            quantityFoil: Number(quantityFoil),
          },
        },
      }),
      invalidatesTags: ['CollectionSummary'],
    }),
    getCardAutocomplete: builder.query<AxiosResponse<CardAutocompleteResponse>, CardAutocompleteOptions>({
      query: ({ name }) => ({
        url: '',
        method: 'POST',
        body: {
          query: cardAutocomplete,
          variables: {
            where: { name: { contains: name ? name.trim() : '', mode: 'insensitive' } },
          },
        },
      }),
    }),
    getCardsFromSubsets: builder.query<AxiosResponse<GetCardsFromSubsetsResponse>, GetCardsFromSubsetsVariables>({
      query: ({ parentSetId }) => ({
        url: '',
        method: 'POST',
        body: {
          query: cardsFromSubsets,
          variables: {
            where: { setId: { parentSetId: { id: { equals: parentSetId } } } },
          },
        },
        providesTags: ['Cards'],
      }),
    }),
    getAllSubsets: builder.query<AxiosResponse<GetAllSubsetsResponse>, GetAllSubsetsVariables>({
      query: ({ parentSetId }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSubsets,
          variables: {
            where: { parentSetId: { id: { equals: parentSetId } } },
          },
        },
        providesTags: ['Sets'],
      }),
    }),
    getAllSubsetsWithGroupId: builder.query<AxiosResponse<GetAllSubsetsByGroupIdResponse>, GetAllSubsetsByGroupIdVariables>({
      query: ({ subsetGroupId }) => ({
        url: '',
        method: 'POST',
        body: {
          query: allSubsetsByGroupId,
          variables: {
            where: { subsetGroupId: { id: { equals: subsetGroupId } } },
          },
        },
        providesTags: ['Sets'],
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
  useGetCostToPurchaseAllQuery,
  useGetCollectionByCardIdLegacyQuery,
  useGetCollectionSummaryLegacyQuery,
  useGetTcgplayerMassImportForUserLegacyQuery,
  useGetSetSummaryLegacyQuery,
  useUpdateCollectionLegacyMutation,
  useGetCardAutocompleteQuery,
  useGetFilteredCollectionSummaryLegacyQuery,
  useGetFilteredCardsSummaryLegacyQuery,
  usePrefetch,
  useGetCardsFromSubsetsQuery,
  useGetAllSubsetsQuery,
  useGetAllSubsetsWithGroupIdQuery,
} = mtgcbApi;

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
  isSubsetGroup: boolean;
  parentSetId: {
    id: string;
    name: string;
  };
}

interface SetResponse {
  sets: Set[];
}

interface AllSetsVariables {
  first: number;
  skip: number;
  sortBy?: string;
  name: string;
  sortByDirection: 'asc' | 'desc';
  setTypes: SetType[];
  setCategories: SetCategory[];
  includeSubsets: boolean;
  includeSubsetGroups: boolean;
}

interface SetBySlugVariables {
  slug: string;
}

interface SetsMetaResponse {
  count: number;
}

interface AllSetsMetaVariables {
  sortBy?: string;
  name: string;
  sortByDirection: 'asc' | 'desc';
  setTypes: SetType[];
  setCategories: SetCategory[];
  includeSubsets: boolean;
  includeSubsetGroups: boolean;
}

interface SetName {
  id: string;
  name: string;
}

interface SetNamesResponse {
  sets: SetName[];
}

interface AllSetNamesVariables {
  sortBy?: string;
}

interface SetTypesResponse {
  setTypes: SetType[];
}

interface AllCardsResponse {
  cards: Card[];
}

interface AllCardsMetaResponse {
  count: number;
}

interface CostToPurchasePerPrice {
  oneOfEachCard: number;
  oneOfEachMythic: number;
  oneOfEachRare: number;
  oneOfEachUncommon: number;
  oneOfEachCommon: number;
  draftCube: number;
}

interface CostsToPurchaseAll {
  setId: number;
  market: CostToPurchasePerPrice;
  low: CostToPurchasePerPrice;
  average: CostToPurchasePerPrice;
  high: CostToPurchasePerPrice;
}

interface CostToPurchaseAllResponse {
  costToPurchaseAll: {
    costToPurchaseAll: CostsToPurchaseAll[];
  };
}

interface CollectionSummaryLegacyResponse {
  collectionSummaryLegacy: {
    userId: number;
    username: string;
    numberOfCardsInMagic: number;
    totalCardsCollected: number;
    uniquePrintingsCollected: number;
    percentageCollected: number;
    totalValue: number;
    collectionSummary: SetSummary[];
  };
}

interface CollectionSummaryLegacyVariables {
  userId: string;
}

interface TcgplayerMassImportForUserLegacyResponse {
  tcgplayerMassImportForUserLegacy: {
    setId: number;
    tcgplayerMassImport: string;
  };
}

interface TcgplayerMassImportForUserLegacyVariables {
  userId: string;
  setId: string;
  includeSubsetsInSets: boolean;
}

interface SetSummaryLegacyResponse {
  setSummaryLegacy: {
    setId: number;
    userId: number;
    username: string;
    cardsInSet: number;
    totalCardsCollectedInSet: number;
    uniquePrintingsCollectedInSet: number;
    percentageCollected: number;
    collection: [
      {
        cardID: number;
        quantityReg: number;
        quantityFoil: number;
      }
    ];
    totalValue: {
      market: number;
      low: number;
      average: number;
      high: number;
    };
  };
}

interface SetSummaryLegacyVariables {
  setId: string;
  userId: string;
}

interface CollectionByCardIdLegacyVariables {
  cardIds: number[];
  userId: string;
}

interface CollectionByCardIdLegacyResponse {
  collectionByCardIdLegacy: {
    userId: number;
    collection: [
      {
        cardID: number;
        quantityReg: number;
        quantityFoil: number;
      }
    ];
  };
}

interface UpdateCollectionLegacyVariables {
  cardId: number;
  mode: 'set' | 'increment';
  quantityRegular: number;
  quantityFoil: number;
  setId: string;
  userId: string;
}

interface UpdateCollectionLegacyResponse {
  updateCollectionLegacy: {
    cardId: number;
    quantityRegular: number;
    quantityFoil: number;
  };
}

interface CardAutocompleteResponse {
  cards: [
    {
      name: string;
      id: number;
      low: number;
      average: number;
      high: number;
      market: number;
      foil: number;
      tcgplayerId: number;
      set: {
        id: string;
        name: string;
        slug: string;
      };
    }
  ];
}

interface CostToCompleteFiltered {
  oneOfEachCard: number;
  oneOfEachMythic: number;
  oneOfEachRare: number;
  oneOfEachUncommon: number;
  oneOfEachCommon: number;
  fourOfEachCard?: number;
  fourOfEachMythic?: number;
  fourOfEachRare?: number;
  fourOfEachUncommon?: number;
  fourOfEachCommon?: number;
  draftCube?: number;
}
interface CollectionSummaryFiltered {
  id: string;
  setId: string;
  totalCardsCollectedInSet: number;
  uniquePrintingsCollectedInSet: number;
  cardsInSet: number;
  percentageCollected: number;
  market: CostToCompleteFiltered;
  low: CostToCompleteFiltered;
  average: CostToCompleteFiltered;
  high: CostToCompleteFiltered;
  name: string;
  slug: string;
  code: string;
  setType: string;
  cardCount: number;
  category: string;
  releasedAt: string;
  sealedProductUrl: string;
  isDraftable: boolean;
  isSubsetGroup: boolean;
  parentSetId: {
    id: string;
    name: string;
  };
}

interface FilteredCollectionSummaryLegacyResponse {
  filteredCollectionSummaryLegacy: {
    userId: number;
    username: string;
    numberOfCardsInMagic: number;
    totalCardsCollected: number;
    uniquePrintingsCollected: number;
    percentageCollected: number;
    totalValue: number;
    collectionSummary: CollectionSummaryFiltered[];
    count: number;
  };
}

interface FilteredCollectionSummaryLegacyVariables {
  userId: string;
  priceType?: 'market' | 'low' | 'average' | 'high' | 'foil';
  first?: number;
  skip?: number;
  search?: string;
  sortBy?: string;
  sortByDirection?: 'asc' | 'desc';
  additionalSortBy?:
    | 'currentValue_asc'
    | 'currentValue_desc'
    | 'costToComplete_asc'
    | 'costToComplete_desc'
    | 'percentageCollected_asc'
    | 'percentageCollected_desc';
  whereSetCompletionStatus?: SetCompletionStatus[];
  setTypes: SetType[];
  setCategories: SetCategory[];
  includeSubsets: boolean;
  includeSubsetGroups: boolean;
  includeSubsetsInSets: boolean;
}

interface FilteredCardsSummaryLegacyVariables {
  userId: string;
  setId?: string;
  first?: number;
  skip?: number;
  sortBy?: string;
  sortByDirection?: 'asc' | 'desc';
  name?: string;
  oracleTextQuery?: string;
  artistQuery?: string;
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardRarities?: CardRarity[];
  cardColors: CardColors;
  showAllPrintings?: boolean;
  cardStatSearches: CardStatSearch[];
  additionalSortBy?:
    | 'currentValue_asc'
    | 'currentValue_desc'
    | 'costToComplete_asc'
    | 'costToComplete_desc'
    | 'percentageCollected_asc'
    | 'percentageCollected_desc';
  additionalWhere: string;
  subsets?: string[];
}

interface FilteredCardsSummaryLegacyResponse {
  filteredCardsSummaryLegacy: {
    userId: string;
    cards: {
      id: number;
      name: string;
      set: {
        id: string;
        name: string;
        slug: string;
      };
      rarity: string;
      manaCost: string;
      convertedManaCost: number;
      oracleTypeLine: string;
      collectorNumber: string;
      tcgplayerId: number;
      low: number;
      average: number;
      high: number;
      market: number;
      foil: number;
      quantityReg: number;
      quantityFoil: number;
    }[];
    count: number;
  };
}
interface GetCardsFromSubsetsResponse {
  cards: Card[];
}

interface GetCardsFromSubsetsVariables {
  parentSetId: string;
}

interface GetAllSubsetsResponse {
  sets: Set[];
}

interface GetAllSubsetsVariables {
  parentSetId: string;
}

interface GetAllSubsetsByGroupIdResponse {
  sets: Set[];
}

interface GetAllSubsetsByGroupIdVariables {
  subsetGroupId: string;
}

const convertToBoolean = (value: string | boolean) => {
  if (value === '1' || value === '0') {
    return value === '1';
  }
  return value as boolean;
};
