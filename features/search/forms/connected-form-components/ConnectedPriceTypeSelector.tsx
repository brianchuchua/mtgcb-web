import { useDispatch, useSelector } from 'react-redux';
import PriceTypeSelector from '../../../../components/search-form-components/PriceTypeSelector';
import { RootState } from '../../../../redux/rootReducer';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedPriceTypeSelectorProps extends ConnectedSearchFormComponentProps {
  setPriceType: any;
}

const ConnectedPriceTypeSelector: React.FC<ConnectedPriceTypeSelectorProps> = ({ reduxSlice, setPriceType }) => {
  const dispatch = useDispatch();
  const { priceType } = useSelector((state: RootState) => state[reduxSlice]);
  const setQueryParameter = useQueryParameter();

  const updatePriceType = (priceTypeValue) => {
    dispatch(setPriceType(priceTypeValue));
    setQueryParameter('price', priceTypeValue);
  };

  return <PriceTypeSelector priceType={priceType} setPriceType={updatePriceType} />;
};

export default ConnectedPriceTypeSelector;
