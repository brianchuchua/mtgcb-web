import { useMemo } from 'react';
import { useSelector } from 'react-redux';
import { useGetFilteredCardsSummaryLegacyQuery } from '../../../network/services/mtgcbApi';
import { RootState } from '../../../redux/rootReducer';

export const useFilteredCardsSummary = (
  userId: string,
  reduxSlice: string,
  hasQuantityFilters: boolean,
  name: string,
  oracleTextQuery: string,
  artistQuery: string,
  first: number,
  skip: number
) => {
  const { sortBy, sortByDirection, cardRarities, cardTypes, cardColors, showAllPrintings, cardStatSearches } = useSelector(
    (state: RootState) => state[reduxSlice]
  );

  const {
    data: filteredCardsSummary,
    isLoading: loadingFilteredCardsSummary,
    isFetching: fetchingFilteredCardsSummary,
  } = useGetFilteredCardsSummaryLegacyQuery(
    {
      userId,
      first,
      skip,
      sortBy,
      sortByDirection,
      name,
      oracleTextQuery,
      artistQuery,
      cardRarities,
      cardTypes,
      cardColors,
      showAllPrintings,
      cardStatSearches,
      additionalWhere: null,
      additionalSortBy: null,
    },
    { skip: !hasQuantityFilters }
  );

  const cardsFromHeavyQuery = filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards;
  const totalResultsFromHeavyQuery = filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.count;
  const collectionByCardIdFromHeavyQuery = useMemo(
    () =>
      filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards?.reduce((acc, curr) => {
        acc[curr.id] = curr;
        return acc;
      }, {} as any),
    [filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards]
  ); // eslint-disable-line @typescript-eslint/no-explicit-any

  const collectionByCardIdWithDefaultsFromHeavyQuery = useMemo(
    () =>
      cardsFromHeavyQuery && collectionByCardIdFromHeavyQuery && !fetchingFilteredCardsSummary
        ? cardsFromHeavyQuery?.reduce((acc, curr) => {
            if (!collectionByCardIdFromHeavyQuery[curr.id]) {
              acc[curr.id] = {
                cardID: Number(curr.id),
                quantityReg: 0,
                quantityFoil: 0,
              };
            } else {
              acc[curr.id] = collectionByCardIdFromHeavyQuery[curr.id];
            }
            return acc;
          }, {} as any)
        : {},
    [collectionByCardIdFromHeavyQuery, cardsFromHeavyQuery, fetchingFilteredCardsSummary]
  );

  return {
    cardsFromHeavyQuery,
    collectionByCardIdWithDefaultsFromHeavyQuery,
    loadingFilteredCardsSummary,
    fetchingFilteredCardsSummary,
    totalResultsFromHeavyQuery,
  };
};
