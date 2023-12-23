import { useCallback, useEffect } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { useGetAllCardsMetaQuery, useGetAllCardsQuery, usePrefetch } from '../../../network/services/mtgcbApi';
import { RootState } from '../../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../../util/useDebounce';
import { usePagination } from '../../../util/usePagination';

export const useCardSearch = (reduxSlice: string, setFormVisibility: any, skipQueryCondition?: boolean) => {
  const {
    searchQuery,
    oracleTextQuery,
    artistQuery,
    cardTypes,
    cardSets,
    cardRarities,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortBy,
    sortByDirection,
    subsets,
  } = useSelector((state: RootState) => state[reduxSlice]);

  const debouncedSearchQuery = useDebounce(searchQuery, searchFieldDebounceTimeMs);
  const debouncedOracleTextQuery = useDebounce(oracleTextQuery, searchFieldDebounceTimeMs);
  const debouncedArtistQuery = useDebounce(artistQuery, searchFieldDebounceTimeMs);

  const { skip, setSkip, first, setFirst, page, setPage, handleTotalResultsChange } = usePagination({
    initialPage: 1,
    initialPageSize: 50,
    localStorageKey: 'numberOfCardsPerPage',
  });

  const dispatch = useDispatch();

  useEffect(() => {
    if (setFormVisibility) {
      dispatch(setFormVisibility({ isFormVisibile: true }));
    }
    return function cleanUpForm() {
      if (setFormVisibility) {
        dispatch(setFormVisibility({ isFormVisibile: false }));
      }
    };
  }, []);

  const { data: cardData, isLoading: isCardDataLoading, isFetching: isCardDataFetching, error: hasCardFetchingError } = useGetAllCardsQuery(
    {
      first,
      skip,
      sortBy,
      name: debouncedSearchQuery,
      oracleTextQuery: debouncedOracleTextQuery,
      artistQuery: debouncedArtistQuery,
      cardSets,
      cardRarities,
      cardTypes,
      cardColors,
      showAllPrintings,
      cardStatSearches,
      sortByDirection,
      subsets,
    },
    {
      skip: skipQueryCondition,
    }
  );

  const {
    data: cardMetaData,
    isLoading: isCardMetaDataLoading,
    isFetching: isCardMetaDataFetching,
    error: hasCardMetaFetchingError,
  } = useGetAllCardsMetaQuery(
    {
      sortBy,
      name: debouncedSearchQuery,
      oracleTextQuery: debouncedOracleTextQuery,
      artistQuery: debouncedArtistQuery,
      cardSets,
      cardRarities,
      cardTypes,
      cardColors,
      showAllPrintings,
      cardStatSearches,
      sortByDirection,
      subsets,
    },
    {
      skip: skipQueryCondition,
    }
  );

  const cards = cardData?.data?.cards;
  const totalResults = cardMetaData?.data?.count;

  useEffect(() => {
    handleTotalResultsChange(totalResults);
  }, [skip, totalResults]);

  const prefetchAllCards = usePrefetch('getAllCards');

  const prefetchNextAllCards = useCallback(() => {
    if (skip + first < totalResults) {
      prefetchAllCards({
        first,
        skip: skip + first,
        sortBy,
        name: debouncedSearchQuery,
        oracleTextQuery: debouncedOracleTextQuery,
        artistQuery: debouncedArtistQuery,
        cardSets,
        cardRarities,
        cardTypes,
        cardColors,
        showAllPrintings,
        cardStatSearches,
        sortByDirection,
        subsets,
      });
    }
  }, [
    prefetchAllCards,
    skip,
    first,
    totalResults,
    sortBy,
    debouncedSearchQuery,
    debouncedOracleTextQuery,
    debouncedArtistQuery,
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  ]);

  useEffect(() => {
    if (skip + first < totalResults) {
      prefetchNextAllCards();
    }
  }, [skip, first, totalResults, prefetchNextAllCards]);

  return {
    cards,
    cardMetaData,
    isCardDataLoading,
    isCardDataFetching,
    isCardMetaDataLoading,
    isCardMetaDataFetching,
    hasCardFetchingError,
    hasCardMetaFetchingError,
    skip,
    setSkip,
    page,
    setPage,
    first,
    setFirst,
    totalResults,
  };
};
