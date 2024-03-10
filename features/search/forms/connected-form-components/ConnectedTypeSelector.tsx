import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import TypeSelector from '../../../../components/search-form-components/TypeSelector';
import { getCardTypes } from '../../../../network/features/browse';
import { RootState } from '../../../../redux/rootReducer';
import { convertCardTypesToString } from '../../../../util/queryStringMappers';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { CardType } from '../../../browse/browseSlice';
import { mapCardTypes } from '../../../browse/forms/mappers';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedTypeSelectorProps extends ConnectedSearchFormComponentProps {
  setCardTypes: any;
}

const ConnectedTypeSelector: React.FC<ConnectedTypeSelectorProps> = ({ reduxSlice, setCardTypes }) => {
  const dispatch = useDispatch();
  const setQueryParameter = useQueryParameter();

  const updateCardTypes = (newCardTypes: CardType[]) => {
    dispatch(setCardTypes({ cardTypes: newCardTypes }));
    setQueryParameter('types', convertCardTypesToString(newCardTypes));
  };

  const [types, setTypes] = useState(null);
  const { cardTypes } = useSelector((state: RootState) => state[reduxSlice]);

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
