export const collectionSettingGroups = (allColumns, priceType) => [
  {
    label: 'Show Columns',
    type: 'tableFilters',
    settings: [
      {
        key: 'id',
        label: 'MTG CB ID',
        getToggleHiddenProps: allColumns[0].getToggleHiddenProps,
      },
      {
        key: 'code',
        label: 'Code',
        getToggleHiddenProps: allColumns[1].getToggleHiddenProps,
      },
      {
        key: 'name',
        label: 'Name',
        getToggleHiddenProps: allColumns[2].getToggleHiddenProps,
      },
      {
        key: 'costsToPurchaseInSet.percentageCollected',
        label: 'Progress',
        getToggleHiddenProps: allColumns[3].getToggleHiddenProps,
      },
      {
        key: `costsToPurchaseInSet[${priceType}].totalValue`,
        label: 'Current Value',
        getToggleHiddenProps: allColumns[4].getToggleHiddenProps,
      },
      {
        key: 'costsToPurchaseInSet.uniquePrintingsCollectedInSet',
        label: 'Unique Cards Collected',
        getToggleHiddenProps: allColumns[5].getToggleHiddenProps,
      },
      {
        key: 'costsToPurchaseInSet.totalCardsCollectedInSet',
        label: 'Total Cards Collected',
        getToggleHiddenProps: allColumns[6].getToggleHiddenProps,
      },
      {
        key: 'cardCount',
        label: 'Cards In Set',
        getToggleHiddenProps: allColumns[7].getToggleHiddenProps,
      },
      {
        key: 'category',
        label: 'Category',
        getToggleHiddenProps: allColumns[8].getToggleHiddenProps,
      },
      {
        key: 'setType',
        label: 'Type',
        getToggleHiddenProps: allColumns[9].getToggleHiddenProps,
      },
      {
        key: 'releasedAt_utc',
        label: 'Release Date',
        getToggleHiddenProps: allColumns[10].getToggleHiddenProps,
      },
    ],
  },
];

export const collectionHiddenColumns = [
  'id',
  'slug',
  'cardCount',
  'costsToPurchaseInSet.uniquePrintingsCollectedInSet',
  'costsToPurchaseInSet.totalCardsCollectedInSet',
];

export const browseSettingGroups = (allColumns, priceType) => [
  {
    label: 'Show Columns',
    type: 'tableFilters',
    settings: [
      {
        key: 'id',
        label: 'MTG CB ID',
        getToggleHiddenProps: allColumns[0].getToggleHiddenProps,
      },
      {
        key: 'code',
        label: 'Code',
        getToggleHiddenProps: allColumns[1].getToggleHiddenProps,
      },
      {
        key: 'name',
        label: 'Name',
        getToggleHiddenProps: allColumns[2].getToggleHiddenProps,
      },
      {
        key: 'cardCount',
        label: 'Cards In Set',
        getToggleHiddenProps: allColumns[3].getToggleHiddenProps,
      },
      {
        key: 'category',
        label: 'Category',
        getToggleHiddenProps: allColumns[4].getToggleHiddenProps,
      },
      {
        key: 'setType',
        label: 'Type',
        getToggleHiddenProps: allColumns[5].getToggleHiddenProps,
      },
      {
        key: 'releasedAt_utc',
        label: 'Release Date',
        getToggleHiddenProps: allColumns[6].getToggleHiddenProps,
      },
    ],
  },
];

export const browseHiddenColumns = ['id', 'slug'];
