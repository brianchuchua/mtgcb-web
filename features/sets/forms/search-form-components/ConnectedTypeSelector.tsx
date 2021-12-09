import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import TypeSelector from '../../../../components/search-form-components/TypeSelector';
import { getCardTypes } from '../../../../network/features/browse';
import { RootState } from '../../../../redux/rootReducer';
import { mapCardTypes } from '../../../browse/forms/mappers';
import { CardType, setCardTypes } from '../../setSlice';

const ConnectedTypeSelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateCardTypes = (newCardTypes: CardType[]) => {
    dispatch(setCardTypes({ cardTypes: newCardTypes }));
  };

  const [types, setTypes] = useState(null);
  const { cardTypes } = useSelector((state: RootState) => state.set);

  useEffect(() => {
    async function fetchTypes() {
      const cardTypesResponse = await getCardTypes();
      const allCardTypes = cardTypesResponse?.data?.data?.cardTypes;
      const allCardTypesMapped = mapCardTypes(allCardTypes);

      setTypes(allCardTypesMapped);
    }
    fetchTypes();
  }, []);

  return <TypeSelector types={types} cardTypes={cardTypes} updateCardTypes={updateCardTypes} />;
};

export default ConnectedTypeSelector;
