function [dates close open tics] = get_prices(stocks_data)

%stocks_data = hist_stock_data(start_date, end_date, file);
close = [];
tics = [];
open=[];
ctr = 0;

% for i = 1:length(stocks_data)
%     len = length(stocks_data(i).Date);
% end

for i = 1:length(stocks_data)
    if(length(stocks_data(i).Date)==756)
        ctr = ctr+1;
        if(ctr==1)
            tics = stocks_data(i).Ticker;
            close = stocks_data(i).AdjClose;
            open = stocks_data(i).Open;
            dates = stocks_data(i).Date;
        end
        tics = strvcat(tics,stocks_data(i).Ticker);
        close = [close stocks_data(i).AdjClose];
        open = [open stocks_data(i).Open];
    end
end

snp_data = hist_stock_data('01012009','31082012','^GSPC');
tics = strvcat(tics,'SNP500');
close = [close snp_data.AdjClose];
open = [open snp_data.Open];
