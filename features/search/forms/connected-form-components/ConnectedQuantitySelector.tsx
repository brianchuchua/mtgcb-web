import { useDispatch, useSelector } from 'react-redux';
import QuantitySelector from '../../../../components/search-form-components/QuantitySelector';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedQuantitySelectorProps extends ConnectedSearchFormComponentProps {
  setQuantity: any;
  quantityType: string;
  label: string;
}

const ConnectedQuantitySelector: React.FC<ConnectedQuantitySelectorProps> = ({ reduxSlice, setQuantity, quantityType, label }) => {
  const dispatch = useDispatch();
  const { [quantityType]: quantity } = useSelector((state: RootState) => state[reduxSlice]);

  const updateQuantity = (newQuantity) => {
    dispatch(setQuantity(newQuantity));
  };

  return <QuantitySelector quantity={quantity} setQuantity={updateQuantity} label={label} />;
};

export default ConnectedQuantitySelector;
