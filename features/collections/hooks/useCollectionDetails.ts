import { useGetSetSummaryLegacyQuery } from '../../../network/services/mtgcbApi';

export const useCollectionDetails = (setData, userId) => {
  const {
    data: setSummaryData,
    isLoading: isSetSummaryLoading,
    isFetching: isSetSummaryFetching,
    error: setSummaryError,
  } = useGetSetSummaryLegacyQuery(
    {
      setId: setData?.data?.sets?.[0]?.id,
      userId,
    },
    { skip: setData?.data?.sets?.[0]?.id == null || userId == null }
  );

  const set = setData?.data?.sets?.[0];
  const setSummary = setSummaryData?.data?.setSummaryLegacy;
  const username = setSummary?.username ?? '';

  const collectionDetails = {
    setName: set?.name,
    setCode: set?.code,
    username,
    userId,
    cardsInSet: setSummary?.cardsInSet,
    totalCardsCollectedInSet: setSummary?.totalCardsCollectedInSet,
    uniquePrintingsCollectedInSet: setSummary?.uniquePrintingsCollectedInSet,
    percentageCollected: setSummary?.percentageCollected,
    totalValue: setSummary?.totalValue,
  };

  return {
    username,
    collectionDetails,
    isSetSummaryLoading,
    isSetSummaryFetching,
    setSummaryError,
  };
};
