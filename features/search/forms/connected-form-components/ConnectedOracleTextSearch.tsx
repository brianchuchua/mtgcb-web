import { useDispatch, useSelector } from 'react-redux';
import OracleTextSearch from '../../../../components/search-form-components/OracleTextSearch';
import { RootState } from '../../../../redux/rootReducer';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedOracleTextSearchProps extends ConnectedSearchFormComponentProps {
  setOracleTextQuery: any;
}

const ConnectedOracleTextSearch: React.FC<ConnectedOracleTextSearchProps> = ({ reduxSlice, setOracleTextQuery }) => {
  const dispatch = useDispatch();
  const { oracleTextQuery } = useSelector((state: RootState) => state[reduxSlice]);
  const setQueryParameter = useQueryParameter();

  const updateOracleTextQuery = (newOracleTextQuery: string) => {
    dispatch(setOracleTextQuery({ oracleTextQuery: newOracleTextQuery }));
    setQueryParameter('oracle', newOracleTextQuery);
  };

  return <OracleTextSearch oracleTextQuery={oracleTextQuery} updateOracleTextQuery={updateOracleTextQuery} />;
};

export default ConnectedOracleTextSearch;
